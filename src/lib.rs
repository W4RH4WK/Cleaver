pub fn my_name() -> &'static str {
    "Cleaver"
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_name() {
        assert_eq!("Cleaver", my_name());
    }
}
