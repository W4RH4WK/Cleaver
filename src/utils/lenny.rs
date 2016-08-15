//! Lenny Face Generator (づσ.σ)づ .
//!
//! This module maintains a set of hardcoded components for [Lenny Faces]. Generate as many of them
//! as you need.
//!
//! [Lenny Faces]: <http://knowyourmeme.com/memes/lenny-face>
//!
//! Source: <https://github.com/Twentysix26/Red-Cogs>

use rand::random;

static EARS: &'static [(&'static str, &'static str)] = include!("lenny_ears.inc");
static EYES: &'static [(&'static str, &'static str)] = include!("lenny_eyes.inc");
static MOUTHS: &'static [&'static str] = include!("lenny_mouths.inc");

/// Generate a new Lenny Face.
pub fn gen() -> String {
    let (ref l_ear, ref r_ear) = EARS[random::<usize>() % EARS.len()];
    let (ref l_eye, ref r_eye) = EYES[random::<usize>() % EYES.len()];
    let mouth = &MOUTHS[random::<usize>() % MOUTHS.len()];
    format!("{}{}{}{}{}", l_ear, l_eye, mouth, r_eye, r_ear)
}

/// (╯°□°）╯︵ ┻━┻
pub fn fliptable() -> String {
    "(╯°□°）╯︵ ┻━┻".to_owned()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn generate() {
        println!("{}", gen());
    }
}
