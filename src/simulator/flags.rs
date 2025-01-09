use std::collections::HashSet;

pub trait Flags {
    fn set_flag(&mut self, flag: char, value: bool);
    fn get_all_flags_sorted(&self) -> String;
    fn has_flag(&self, flag: &char) -> bool;
}

impl Flags for HashSet<char> {
    fn set_flag(&mut self, flag: char, value: bool) {
        if value {
            self.insert(flag);
        } else {
            self.remove(&flag);
        }
    }

    fn get_all_flags_sorted(&self) -> String {
        let mut flags: Vec<&char> = self.iter().collect();
        flags.sort();
        String::from_iter(flags)
    }

    fn has_flag(&self, flag: &char) -> bool {
        self.contains(flag)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use crate::simulator::flags::Flags;

    #[test]
    fn test_get_and_set_flag() {
        let mut flags = HashSet::<char>::new();
        flags.set_flag('c', true);
        flags.set_flag('b', true);
        flags.set_flag('x', true);

        assert_eq!("bcx".to_string(), flags.get_all_flags_sorted());
    }
}
