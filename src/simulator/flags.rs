use std::collections::HashMap;

pub trait Flags {
    fn set_flag(&mut self, flag: char, value: bool) -> Option<bool>;
    fn get_all_flags_sorted(&self) -> String;
}

impl Flags for HashMap<char, bool> {
    fn set_flag(&mut self, flag: char, value: bool) -> Option<bool> {
        self.insert(flag, value)
    }

    fn get_all_flags_sorted(&self) -> String {
        let mut flags: Vec<&char> = self.keys().collect();
        flags.sort();
        String::from_iter(flags)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use crate::simulator::flags::Flags;

    #[test]
    fn test_get_and_set_flag() {
        let mut flags = HashMap::<char, bool>::new();
        flags.set_flag('c', true);
        flags.set_flag('b', true);
        flags.set_flag('x', true);

        assert_eq!("bcx".to_string(), flags.get_all_flags_sorted());
    }
}
