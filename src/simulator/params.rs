#[derive(PartialEq, Eq, Debug)]
pub enum Rmv {
    Register(String),
    Value(u16),
    Memory(String),
}

impl Rmv {
    pub fn from_str(src: &str) -> Rmv {
        let binding = src.replace("word", "");
        let sanitized_src = binding.trim();
        if sanitized_src.starts_with("[") {
            return Rmv::Memory(sanitized_src.trim_start_matches("[").trim_end_matches("]").to_string());
        }
        match sanitized_src.parse::<u16>() {
            Ok(u) => Rmv::Value(u),
            Err(_) => Rmv::Register(src.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Rmv;

    #[test]
    fn test_rmv_from_register() {
        assert_eq!(Rmv::Register("ax".to_string()), Rmv::from_str("ax"));
        assert_eq!(Rmv::Register("bx".to_string()), Rmv::from_str("bx"));
    }

    #[test]
    fn test_rmv_from_value() {
        assert_eq!(Rmv::Value(1000), Rmv::from_str("1000"));
        assert_eq!(Rmv::Value(20), Rmv::from_str("20"));
    }

    #[test]
    fn test_rmv_from_memory() {
        assert_eq!(Rmv::Memory("bp + si".to_string()), Rmv::from_str("[bp + si]"));
        assert_eq!(Rmv::Memory("2012".to_string()), Rmv::from_str("[2012"));
    }

    #[test]
    fn test_rmv_from_dirty_string() {
        assert_eq!(Rmv::Memory("bp + si".to_string()), Rmv::from_str("word [bp + si]"));
        assert_eq!(Rmv::Value(1000), Rmv::from_str("  1000"));
    }
}
