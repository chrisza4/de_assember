use std::collections::HashMap;

pub trait RegisterSet {
    fn get_by_reg_name(&self, register: &str) -> Option<u16>;
    fn insert_by_reg_name(&mut self, register: &str, value: u16) -> Option<u16>;
}

impl RegisterSet for HashMap<String, u16> {
    fn get_by_reg_name(&self, register: &str) -> Option<u16> {
        match register {
            register  if  register.ends_with("l") => {
                let real_register = register.replace("l", "x");
                self.get(&real_register).copied().map(|x| x & 0x00ff)
            },
            register if register.ends_with("h") => {
                let real_register = register.replace("h", "x");
                self.get(&real_register).copied().map(|x| (x & 0xff00) >> 8)
            },
            _ => self.get(register).copied()
        }
    }

    fn insert_by_reg_name(&mut self, register: &str, value: u16) -> Option<u16> {
        match register {
            register  if  register.ends_with("l") => {
                let real_register = register.replace("l", "x");
                let current_val = self.get(&real_register).unwrap_or(&0);
                self.insert(real_register, (current_val & 0xff00) | value)
            },
            register  if  register.ends_with("h") => {
                let real_register = register.replace("h", "x");
                let current_val = self.get(&real_register).unwrap_or(&0);
                self.insert(real_register, (current_val & 0x00ff) | (value << 8))
            },
            _ => self.insert(register.to_string(), value)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use crate::simulator::register_set::RegisterSet;

    #[test]
    fn test_get_whole_register() {
        let registers = HashMap::from([("ax".to_string(), 4u16)]);

        assert_eq!(registers.get_by_reg_name("ax"), Some(4));
    }

    #[test]
    fn test_get_low_register() {
        let registers = HashMap::from([("ax".to_string(), 0x2321)]);

        assert_eq!(registers.get_by_reg_name("al"), Some(0x0021));
    }

    #[test]
    fn test_get_high_register() {
        let registers = HashMap::from([("ax".to_string(), 0x5678)]);

        assert_eq!(registers.get_by_reg_name("ah"), Some(0x0056));
    }

    #[test]
    fn insert_register_normal() {
        let mut registers = HashMap::from([("ax".to_string(), 0x5678)]);
        registers.insert_by_reg_name("bx", 5);

        assert_eq!(registers.get_by_reg_name("ax"), Some(0x5678));
        assert_eq!(registers.get_by_reg_name("bx"), Some(5));
    }

    #[test]
    fn insert_register_low() {
        let mut registers = HashMap::from([("ax".to_string(), 0x5678)]);
        registers.insert_by_reg_name("al", 0x0033);

        assert_eq!(registers.get_by_reg_name("ax"), Some(0x5633));
    }

    #[test]
    fn insert_register_high() {
        let mut registers = HashMap::from([("ax".to_string(), 0x5678)]);
        registers.insert_by_reg_name("ah", 0x0033);

        assert_eq!(registers.get_by_reg_name("ax"), Some(0x3378));
    }
}
