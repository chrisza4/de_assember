use std::collections::HashMap;

trait RegisterSet {
    fn get_by_reg_name(&self, register: &str) -> Option<u16>;
    fn insert_by_reg_name(&self, register: &str) -> Option<u16>;
}

impl RegisterSet for HashMap<String, u16> {
    fn get_by_reg_name(&self, register: &str) -> Option<u16> {
        if register.ends_with("l") {
            let real_register = register.replace("l", "x");
            return self.get(&real_register).copied().map(|x| x & 0x00ff);
        }
        self.get(register).copied()        
    }

    fn insert_by_reg_name(&self, register: &str) -> Option<u16> {
        todo!()
    }
}

mod tests {
    use std::collections::HashMap;

    use crate::simulator::register_set::RegisterSet;

    #[test]
    fn test_get_whole_register() {
        let registers = HashMap::from([("ax".to_string(), 4u16)]);

        assert_eq!(registers.get_by_reg_name("ax"), Some(4));
    }

    #[test]
    fn test_get_lower_register() {
        let registers = HashMap::from([("ax".to_string(), 0x2222)]);

        assert_eq!(registers.get_by_reg_name("al"), Some(0x0022));
    }
}
