#[derive(PartialEq, Eq, Debug)]
pub enum Rmv {
    Register(String),
    Value(u16),
    Memory(u16),
}

impl Rmv {
    fn from_str(src: &str) -> Rmv {
        Rmv::Register(src.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::Rmv;

    #[test]
    fn test_rmv_from_register() {
        assert_eq!(Rmv::Register("ax".to_string()), Rmv::from_str("ax"));
    }
}
