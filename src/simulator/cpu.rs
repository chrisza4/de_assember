use std::collections::{HashMap, HashSet};

use crate::binary::combined_u8;

use super::{params::Rmv, register_set::RegisterSet};

pub struct Cpu {
    pub register: HashMap<String, u16>,
    pub flags: HashSet<char>,
    pub pointer: usize,
    pub memory: Vec<u8>,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            register: Default::default(),
            flags: Default::default(),
            pointer: Default::default(),
            memory: vec![0; 1048527],
        }
    }
}

pub trait RmvStore {
    fn get_by_rmv(&self, address: Rmv) -> u16;
    fn set_by_rmv(&mut self, address: Rmv);
}

impl RmvStore for Cpu {
    fn get_by_rmv(&self, address: Rmv) -> u16 {
        match address {
            Rmv::Register(register) => self.register.get_by_reg_name(&register).unwrap_or(0),
            Rmv::Value(x) => x,
            Rmv::Memory(memory_address_str) => {
                let memory_index: u16 = memory_address_str
                    .split("+")
                    .map(|x| {
                        let memory_or_register = x.trim();
                        match memory_or_register.parse::<u16>() {
                            Ok(v) => v,
                            Err(_) => self
                                .register
                                .get_by_reg_name(memory_or_register)
                                .unwrap_or(0),
                        }
                    })
                    .sum();
                let memory_index = memory_index as usize;
                let first_byte = self.memory[memory_index];
                let second_byte = self.memory[memory_index + 1];
                combined_u8(first_byte, second_byte)
            }
        }
    }

    fn set_by_rmv(&mut self, address: Rmv) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::{Cpu, RmvStore};
    use crate::{binary::combined_u8, simulator::params::Rmv};

    #[test]
    fn test_get_by_rmv_value() {
        let cpu = Cpu::default();
        assert_eq!(cpu.get_by_rmv(Rmv::Value(30)), 30);
    }

    #[test]
    fn test_get_by_rmv_memory() {
        let mut cpu = Cpu::default();
        cpu.memory[32] = 20;
        cpu.memory[33] = 30;
        let expected = combined_u8(20, 30);
        assert_eq!(cpu.get_by_rmv(Rmv::Memory("32".to_string())), expected);
    }

    #[test]
    fn test_get_by_rmv_memory_register_combined() {
        let mut cpu = Cpu::default();
        cpu.register.insert("ax".to_string(), 10);
        cpu.memory[32] = 20;
        cpu.memory[33] = 30;
        let expected = combined_u8(20, 30);
        assert_eq!(cpu.get_by_rmv(Rmv::Memory("ax + 22".to_string())), expected);
    }

    #[test]
    fn test_get_by_rmv_register() {
        let mut cpu = Cpu::default();
        cpu.register.insert("ax".to_string(), 10);
        assert_eq!(cpu.get_by_rmv(Rmv::Register("ax".to_string())), 10);
    }
}
