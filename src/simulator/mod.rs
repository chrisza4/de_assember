use flags::Flags;
use register_set::RegisterSet;

use crate::deassembler::decoder::decode;
use std::{
    cmp::min,
    collections::{HashMap, HashSet},
};

pub mod flags;
mod register_set;

pub struct Cpu {
    pub register: HashMap<String, u16>,
    pub flags: HashSet<char>,
    pub pointer: usize,
}

trait RegisterBits {
    fn has_signed_bit(self) -> bool;
}

impl RegisterBits for u16 {
    fn has_signed_bit(self) -> bool {
        const SIGNED_FLAG_MUST_BE_OVER: u16 = 2u16.pow(7);
        self >= SIGNED_FLAG_MUST_BE_OVER
    }
}

#[allow(dead_code)]
pub fn simulate_from_code(code: String) -> Result<Cpu, ParseAssemblyError> {
    let mut result = Cpu {
        register: HashMap::<String, u16>::new(),
        flags: HashSet::new(),
        pointer: 0,
    };
    let lines = code.split('\n');
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        match simulate_line(&mut result, line) {
            Ok(()) => (),
            Err(e) => return Err(e),
        }
    }
    Ok(result)
}

pub fn simulate_from_binary(binary: &[u8]) -> Result<Cpu, ParseAssemblyError> {
    let mut result = Cpu {
        register: HashMap::<String, u16>::new(),
        flags: HashSet::new(),
        pointer: 0,
    };
    let mut count = 0;
    while binary.get(result.pointer).is_some() {
        count += 1;
        let end = min(binary.len(), result.pointer + 6);
        let current_chunk = Vec::<u8>::from(&binary[result.pointer..end]);
        let (asm_instruction, bytes_consumed) = decode(&current_chunk).unwrap();
        result.pointer += usize::from(bytes_consumed);
        match simulate_line(&mut result, &asm_instruction) {
            Ok(()) => (),
            Err(e) => return Err(e),
        }
        if count > 10000 {
            panic!("Too many execution. Possibly bug")
        }
    }
    Ok(result)
}

pub fn simulate_line(state: &mut Cpu, line: &str) -> Result<(), ParseAssemblyError> {
    let assembly = parse_assembly_code(line);
    println!("Asm: {}", line);
    match assembly {
        Ok(Assembly::Mov(register, RegisterOrValue::Value(val))) => {
            state.register.insert_by_reg_name(&register, val);
        }
        Ok(Assembly::Mov(register, RegisterOrValue::Register(from_reg))) => {
            let val = state.register.get_by_reg_name(&from_reg).unwrap();

            state.register.insert_by_reg_name(&register, val);
        }
        Ok(Assembly::Sub(register, RegisterOrValue::Register(from_reg))) => {
            let current_val = state.register.get_by_reg_name(&register).unwrap_or(0);
            let val = state.register.get_by_reg_name(&from_reg).unwrap_or(0);
            let new_val = current_val.wrapping_sub(val);
            state.register.insert_by_reg_name(&register, new_val);
            state.flags.set_flag('z', new_val == 0);
            state.flags.set_flag('s', new_val.has_signed_bit());
        }
        Ok(Assembly::Sub(register, RegisterOrValue::Value(val))) => {
            let current_val = state.register.get_by_reg_name(&register).unwrap_or(0);
            let new_val = current_val.wrapping_sub(val);
            state.register.insert_by_reg_name(&register, new_val);
            state.flags.set_flag('z', new_val == 0);
            state.flags.set_flag('s', new_val.has_signed_bit());
        }
        Ok(Assembly::Cmp(register, RegisterOrValue::Register(from_reg))) => {
            let current_val = state.register.get_by_reg_name(&register).unwrap_or(0);
            let val = state.register.get_by_reg_name(&from_reg).unwrap_or(0);
            let new_val = current_val.wrapping_sub(val);
            state.flags.set_flag('z', new_val == 0);
            state.flags.set_flag('s', new_val.has_signed_bit());
        }
        Ok(Assembly::Cmp(register, RegisterOrValue::Value(val))) => {
            let current_val = state.register.get_by_reg_name(&register).unwrap_or(0);
            let new_val = current_val.wrapping_sub(val);
            state.flags.set_flag('z', new_val == 0);
            state.flags.set_flag('s', new_val.has_signed_bit());
        }
        Ok(Assembly::Add(register, RegisterOrValue::Register(from_reg))) => {
            let current_val = state.register.get_by_reg_name(&register).unwrap_or(0);
            let val = state.register.get_by_reg_name(&from_reg).unwrap_or(0);
            let new_val = current_val.wrapping_add(val);
            state.register.insert_by_reg_name(&register, new_val);
            state.flags.set_flag('z', new_val == 0);
            state.flags.set_flag('s', new_val.has_signed_bit());
        }
        Ok(Assembly::Add(register, RegisterOrValue::Value(val))) => {
            let current_val = state.register.get_by_reg_name(&register).unwrap_or(0);
            let new_val = current_val.wrapping_add(val);
            state.register.insert_by_reg_name(&register, new_val);
            state.flags.set_flag('z', new_val == 0);
            state.flags.set_flag('s', new_val.has_signed_bit());
        }
        Ok(Assembly::Jnz(value)) => {
            if !state.flags.has_flag(&'z') {
                state.pointer = ((state.pointer as isize) + (value as isize))
                    .try_into()
                    .unwrap_or(0);
            }
        }
        Ok(Assembly::Bit) => (),
        Err(e) => return Err(e),
    };
    Ok(())
}

enum RegisterOrValue {
    Register(String),
    Value(u16),
}

enum Assembly {
    Mov(String, RegisterOrValue),
    Sub(String, RegisterOrValue),
    Cmp(String, RegisterOrValue),
    Add(String, RegisterOrValue),
    Jnz(i8),
    Bit,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParseAssemblyError {
    InvalidParams(String),
    Unknown,
}

fn strip_comment(code: &str) -> &str {
    if code.contains(';') {
        code.split_once(';').unwrap().0
    } else {
        code
    }
}

fn parse_2_params<'a>(
    command: &str,
    params: Option<&'a str>,
) -> Result<(&'a str, &'a str), ParseAssemblyError> {
    let Some(params) = params else {
        return Err(ParseAssemblyError::InvalidParams(format!(
            "{command} required a parameter"
        )));
    };
    let move_params: Vec<&str> = params.split(',').collect();
    let param1 = move_params.first().unwrap();
    let Some(param2) = move_params.get(1).map(|x| x.trim()) else {
        return Err(ParseAssemblyError::InvalidParams(format!(
            "{command} required 2 parameters"
        )));
    };

    Ok((*param1, param2))
}

fn parse_assembly_code(code: &str) -> Result<Assembly, ParseAssemblyError> {
    let code_without_comment = strip_comment(code);
    let command_and_params = code_without_comment.trim().split_once(' ');
    let command = command_and_params.map(|x| x.0).unwrap_or(code);
    match command {
        "mov" => {
            let (register_to_mov_to, value) =
                parse_2_params(command, command_and_params.map(|x| x.1))?;
            match value.parse::<u16>() {
                Ok(u) => Ok(Assembly::Mov(
                    register_to_mov_to.to_string(),
                    RegisterOrValue::Value(u),
                )),
                Err(_) => Ok(Assembly::Mov(
                    register_to_mov_to.to_string(),
                    RegisterOrValue::Register(value.to_string()),
                )),
            }
        }
        "sub" => {
            let (register, value) = parse_2_params(command, command_and_params.map(|x| x.1))?;
            match value.parse::<u16>() {
                Ok(u) => Ok(Assembly::Sub(
                    register.to_string(),
                    RegisterOrValue::Value(u),
                )),
                Err(_) => Ok(Assembly::Sub(
                    register.to_string(),
                    RegisterOrValue::Register(value.to_string()),
                )),
            }
        }
        "cmp" => {
            let (register, value) = parse_2_params(command, command_and_params.map(|x| x.1))?;
            match value.parse::<u16>() {
                Ok(u) => Ok(Assembly::Cmp(
                    register.to_string(),
                    RegisterOrValue::Value(u),
                )),
                Err(_) => Ok(Assembly::Cmp(
                    register.to_string(),
                    RegisterOrValue::Register(value.to_string()),
                )),
            }
        }

        "add" => {
            let (register, value) = parse_2_params(command, command_and_params.map(|x| x.1))?;
            match value.parse::<u16>() {
                Ok(u) => Ok(Assembly::Add(
                    register.to_string(),
                    RegisterOrValue::Value(u),
                )),
                Err(_) => Ok(Assembly::Add(
                    register.to_string(),
                    RegisterOrValue::Register(value.to_string()),
                )),
            }
        }
        "bits" => Ok(Assembly::Bit),
        "jnz" => {
            let value = command_and_params.map(|x| x.1.parse::<i8>());
            match value {
                Some(Ok(value)) => Ok(Assembly::Jnz(value)),
                Some(Err(_)) => Err(ParseAssemblyError::InvalidParams(
                    "Params of jnz invalid".to_string(),
                )),
                None => Err(ParseAssemblyError::Unknown),
            }
        }
        _ => {
            println!("Parse error for {:?}", command);
            Err(ParseAssemblyError::Unknown)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use crate::simulator::{
        flags::Flags, register_set::RegisterSet, simulate_line, Cpu, ParseAssemblyError,
    };

    use super::{simulate_from_binary, simulate_from_code};
    #[test]
    fn test_simulate_simple_mov() {
        let code = "mov ax, 3";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
    }

    #[test]
    fn test_simulate_multiline_mov() {
        let code = "mov ax, 3
      mov bx, 4";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 4);
    }

    #[test]
    fn test_simulate_multiline_mov_with_comment() {
        let code = "mov ax, 3 ;comment
      mov bx, 4";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 4);
    }

    #[test]
    fn test_simulate_mov_register() {
        let code = "mov ax, 3
      mov bx, ax";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 3);
    }

    #[test]
    fn test_simulate_multiline_mov_with_blank_lines() {
        let code = "mov ax, 3

      mov bx, 4";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 4);
    }

    #[test]
    fn test_bit_do_nothing() {
        let code = "bits 16";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        let result = simulate_line(&mut current_state, code);
        assert_eq!(result, Ok(()));
        assert_eq!(current_state.register.len(), 0);
    }

    #[test]
    fn test_simulate_line_simple_mov() {
        let code = "mov ax, 3";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        simulate_line(&mut current_state, code).unwrap();

        assert_eq!(current_state.register["ax"], 3);
    }

    #[test]
    fn test_simulate_line_mov_register() {
        let code = "mov ax, bx";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        current_state.register.insert("bx".into(), 3);
        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 3);
    }

    #[test]
    fn test_mov_err_without_params() {
        let code = "mov";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        let result = simulate_line(&mut current_state, code);

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "mov required a parameter".to_string()
            ))
        );
    }

    #[test]
    fn test_mov_err_without_value() {
        let code = "mov ax";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        let result = simulate_line(&mut current_state, code);

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "mov required 2 parameters".to_string()
            ))
        );
    }

    #[test]
    fn test_sub_err_without_params() {
        let code = "sub";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        // let result = simue(code.to_string());
        let result = simulate_line(&mut current_state, code);

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "sub required a parameter".to_string()
            ))
        );
    }

    #[test]
    fn test_sub_err_without_value() {
        let code = "sub ax";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        let result = simulate_line(&mut current_state, code);

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "sub required 2 parameters".to_string()
            ))
        );
    }

    #[test]
    fn test_simulate_sub_from_existing_state_no_flag() {
        let code = "sub ax, 2";
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16)]),
            flags: HashSet::new(),
            pointer: 0,
        };

        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 4);
        assert!(!current_state.flags.get_all_flags_sorted().contains("z"));
    }

    #[test]
    fn test_simulate_sub_from_existing_state_with_flag() {
        let code = "sub ax, 6";
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16)]),
            flags: HashSet::new(),
            pointer: 0,
        };

        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 0);
        assert!(current_state.flags.get_all_flags_sorted().contains("z"));
    }

    #[test]
    fn test_simulate_sub_from_existing_state_with_flag_and_reg() {
        let code = "sub ax, bx";
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16), ("bx".to_string(), 6u16)]),
            flags: HashSet::new(),
            pointer: 0,
        };

        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 0);
        assert!(current_state.flags.get_all_flags_sorted().contains("z"));
    }

    #[test]
    fn test_simulate_sub_from_existing_state_no_flag_and_reg() {
        let code = "sub ax, bx";
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16), ("bx".to_string(), 5u16)]),
            flags: HashSet::new(),
            pointer: 0,
        };

        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 1);
        assert!(!current_state.flags.get_all_flags_sorted().contains("z"));
    }

    #[test]
    fn test_simulate_sub_signed_flag() {
        let mut current_state = Cpu {
            register: HashMap::from_iter([
                ("ax".to_string(), 6u16),
                ("bx".to_string(), 65500u16),
                ("cx".to_string(), 2u16),
            ]),
            flags: HashSet::new(),
            pointer: 0,
        };

        let test_cases = [
            (
                "sub ax, 7",
                "ax",
                65535,
                "s",
                "should assigned signed flag when sub result is minus",
            ),
            (
                "sub bx, 1",
                "bx",
                65499u16,
                "s",
                "should assigned signed flag when sub result have signed bit",
            ),
            (
                "sub cx, 1",
                "cx",
                1u16,
                "",
                "should not assigned signed flag",
            ),
        ];

        for test_case in test_cases {
            let (code, register, expected_value, expected_flags, err_message) = test_case;
            simulate_line(&mut current_state, code).unwrap();
            assert_eq!(
                current_state.register[register], expected_value,
                "{}",
                err_message
            );
            assert_eq!(
                current_state.flags.get_all_flags_sorted(),
                expected_flags,
                "{}",
                err_message
            );
        }
    }

    #[test]
    fn test_simulate_cmp() {
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16), ("bx".to_string(), 5u16)]),
            flags: HashSet::new(),
            pointer: 0,
        };
        let test_cases = [
            ("cmp ax, 6", "z"),
            ("cmp ax, 5", ""),
            ("cmp ax, bx", ""),
            ("cmp ax, 7", "s"),
        ];
        for test_case in test_cases {
            let (code, expected_flags) = test_case;
            simulate_line(&mut current_state, code).unwrap();
            assert_eq!(current_state.flags.get_all_flags_sorted(), expected_flags);
        }
    }

    #[test]
    fn test_simulate_add() {
        let mut current_state = Cpu {
            register: HashMap::from_iter([
                ("ax".to_string(), 6u16),
                ("bx".to_string(), 5u16),
                ("cx".to_string(), 1u16),
                ("dx".to_string(), 65535u16),
            ]),
            flags: HashSet::new(),
            pointer: 0,
        };
        let test_cases = [
            ("add ax, 7", "ax", 13u16, ""),
            ("add bx, ax", "bx", 18, ""),
            ("add cx, 32767", "cx", 32768, "s"),
            ("add dx, 1", "dx", 0, "z"),
        ];
        for test_case in test_cases {
            let (code, register, expected_value, expected_flag) = test_case;
            simulate_line(&mut current_state, code).unwrap();
            assert_eq!(
                current_state.register.get_by_reg_name(register),
                Some(expected_value),
                "Failed on test {}",
                code
            );
            assert_eq!(
                current_state.flags.get_all_flags_sorted(),
                expected_flag,
                "Failed on test {}",
                code
            );
        }
    }

    #[test]
    fn test_simulate_line_mov_with_existing_state() {
        let code = "mov bx, 4";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
            pointer: 0,
        };
        current_state.register.insert("bx".into(), 3);
        simulate_line(&mut current_state, code).unwrap();

        assert_eq!(current_state.register["bx"], 4);
    }

    #[test]
    fn test_simulate_from_binary() {
        let binary = [185, 3, 0, 184, 4, 0, 137, 195]; // From asm/test_simulation
        let result = simulate_from_binary(&binary).unwrap();

        assert_eq!(result.register["ax"], 4);
        assert_eq!(result.register["bx"], 4);
        assert_eq!(result.register["cx"], 3);
        assert_eq!(result.pointer, 8);
    }

    #[test]
    fn test_simulate_jnz() {
        // Original source
        // =======
        // mov ax, 6
        // mov bx, 0
        // start:
        // add bx, 1
        // sub ax, 2
        // jnz start

        let binary = [
            0b10111000, 0b00000110, 0b00000000, 0b10111011, 0b00000000, 0b00000000, 0b10000011,
            0b11000011, 0b00000001, 0b10000011, 0b11101000, 0b00000010, 0b01110101, 0b11111000,
        ];

        let result = simulate_from_binary(&binary).unwrap();

        assert_eq!(result.register["ax"], 0);
        assert_eq!(result.register["bx"], 3);
        assert_eq!(result.pointer, 14);
    }
}
