use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::result;

use ::front::Position;
use ::front::FrontendError;
use ::diagnostics as diag;
use super::ast;

pub type Result<T> = result::Result<T, SymbolError>;

type SymbolTable = HashMap<String, Rc<ast::Variable>>;

#[derive(Debug)]
pub struct Context {
    pub id_counter: u32,
    pub filename: String,
    pub function_name: String,
    pub scopes: Vec<SymbolTable>,
    pub log: Option<diag::symbols::Log>,
}

impl Context {
    pub fn new(function: &ast::Node<ast::Function>, config: &Option<diag::Config>) -> Context {
        Context {
            id_counter: 1,
            filename: function.node.filename.clone(),
            function_name: function.node.name.clone(),
            scopes: vec![SymbolTable::new()],
            log: if config.as_ref().map_or(false, |c| c.dump_symbol_table) {
                Some(diag::symbols::Log::new(function, config))
            } else {
                None
            },
        }
    }

    pub fn lookup(&self, var: &str) -> Result<Rc<ast::Variable>> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(var) {
                return Ok(v.clone());
            }
        }
        Err(SymbolError::UnknownVariable {
            pos: Position::default(),
            var: var.to_owned(),
            filename: self.filename.clone(),
        })
    }

    pub fn insert(&mut self, var: &mut Rc<ast::Variable>) -> Result<Rc<ast::Variable>> {
        // check for duplicate
        if self.scopes.last().unwrap().contains_key(&var.name) {
            return Err(SymbolError::AlreadyDeclared {
                pos: Position::default(),
                var: var.name.clone(),
                filename: self.filename.clone(),
            });
        }

        // insert new one
        let new = Rc::new(ast::Variable {
            id: self.id_counter,
            type_: var.type_,
            name: var.name.clone(),
        });
        self.scopes.last_mut().unwrap().insert(var.name.clone(), new.clone());
        self.id_counter += 1;

        if let Some(ref mut log) = self.log {
            log.insert(&new);
        }

        // return reference to final variable
        Ok(new)
    }
}

pub fn symbolize(function: &mut ast::Node<ast::Function>) -> Result<()> {
    symbolize_with_diag(function, &None)
}

pub fn symbolize_with_diag(function: &mut ast::Node<ast::Function>,
                           config: &Option<diag::Config>)
                           -> Result<()> {
    let mut ctx = Context::new(function, config);

    if let Some(ref mut log) = ctx.log {
        log.function(function);
    }

    // add arugments to root scope
    let pos = function.pos;
    for arg in &mut function.node.args {
        *arg = try!(ctx.insert(arg).map_err(|err| err.pos(pos)));
    }

    try!(symbolize_statement(&mut ctx, &mut function.node.body));

    if let Some(ref mut log) = ctx.log {
        log.scope_close();
    }

    Ok(())
}

fn symbolize_statement(ctx: &mut Context, stmt: &mut ast::Node<ast::Statement>) -> Result<()> {

    // log creation of new scope, must be done before destructuring `stmt`
    if let Some(ref mut log) = ctx.log {
        if let ast::Statement::Compound { .. } = stmt.node {
            log.scope_open(stmt);
        }
    }

    let pos = stmt.pos;
    match stmt.node {
        ast::Statement::Expression { ref mut expr } => symbolize_expression(ctx, expr),
        ast::Statement::Declaration { ref mut var } => {
            *var = try!(ctx.insert(var).map_err(|err| err.pos(pos)));
            Ok(())
        }
        ast::Statement::Assignment { ref mut var, ref mut expr } => {
            *var = try!(ctx.lookup(&var.name).map_err(|err| err.pos(expr.pos)));
            symbolize_expression(ctx, expr)
        }
        ast::Statement::If { ref mut cond, ref mut on_true, ref mut on_false } => {
            try!(symbolize_expression(ctx, cond));
            try!(symbolize_statement(ctx, on_true));
            if let Some(ref mut stmt) = *on_false {
                symbolize_statement(ctx, stmt)
            } else {
                Ok(())
            }
        }
        ast::Statement::While { ref mut cond, ref mut body } => {
            try!(symbolize_expression(ctx, cond));
            symbolize_statement(ctx, body)
        }
        ast::Statement::Return { expr: Some(ref mut e) } => symbolize_expression(ctx, e),
        ast::Statement::Compound { ref mut stmts } => {
            // open up new scope
            ctx.scopes.push(SymbolTable::new());

            for stmt in stmts.iter_mut() {
                try!(symbolize_statement(ctx, stmt));
            }

            // scope done
            ctx.scopes.pop();
            if let Some(ref mut log) = ctx.log {
                log.scope_close();
            }

            Ok(())
        }
        _ => Ok(()),
    }
}

fn symbolize_expression(ctx: &mut Context, expr: &mut ast::Node<ast::Expression>) -> Result<()> {
    let pos = expr.pos;
    match expr.node {
        ast::Expression::Variable { ref mut var } => {
            *var = try!(ctx.lookup(&var.name).map_err(|err| err.pos(pos)));
            Ok(())
        }
        ast::Expression::Call { ref mut args, .. } => {
            for arg in args.iter_mut() {
                try!(symbolize_expression(ctx, arg));
            }
            Ok(())
        }
        ast::Expression::Binary { ref mut left, ref mut right, .. } => {
            try!(symbolize_expression(ctx, left));
            symbolize_expression(ctx, right)
        }
        ast::Expression::Unary { ref mut expr, .. } |
        ast::Expression::Parenthesis { ref mut expr } => symbolize_expression(ctx, expr),
        _ => Ok(()),
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum SymbolError {
    UnknownVariable {
        pos: Position,
        var: String,
        filename: String,
    },
    AlreadyDeclared {
        pos: Position,
        var: String,
        filename: String,
    },
}

impl SymbolError {
    fn pos(mut self, position: Position) -> SymbolError {
        match self {
            SymbolError::UnknownVariable { ref mut pos, .. } |
            SymbolError::AlreadyDeclared { ref mut pos, .. } => *pos = position,
        };
        self
    }
}

impl fmt::Display for SymbolError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SymbolError::UnknownVariable { ref var, .. } => write!(f, "UnknownVariable: `{}`", var),
            SymbolError::AlreadyDeclared { ref var, .. } => write!(f, "AlreadyDeclared: `{}`", var),
        }
    }
}

impl Error for SymbolError {
    fn description(&self) -> &str {
        match *self {
            SymbolError::UnknownVariable { .. } => "unknown variable used",
            SymbolError::AlreadyDeclared { .. } => "variable already declared in this scope",
        }
    }
}

impl From<SymbolError> for FrontendError {
    fn from(err: SymbolError) -> FrontendError {
        let (pos, filename) = match err {
            SymbolError::UnknownVariable { pos, ref filename, .. } |
            SymbolError::AlreadyDeclared { pos, ref filename, .. } => (pos, filename.clone()),
        };
        FrontendError {
            pos: pos,
            filename: filename,
            msg: err.to_string(),
            cause: Some(Box::new(err)),
        }
    }
}
