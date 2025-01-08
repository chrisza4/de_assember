use flags::Flags;
use register_set::RegisterSet;

use crate::deassembler::decoder::decode;
use std::{
    cmp::min,
    collections::{HashMap, HashSet},
};

mod flags;
mod register_set;

pub struct Cpu {
    pub register: HashMap<String, u16>,
    pub flags: HashSet<char>,
}

#[allow(dead_code)]
pub fn simulate_from_code(code: String) -> Result<Cpu, ParseAssemblyError> {
    let mut result = Cpu {
        register: HashMap::<String, u16>::new(),
        flags: HashSet::new(),
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
    };
    let mut pointer = 0;
    while binary.get(pointer).is_some() {
        let end = min(binary.len(), pointer + 6);
        let current_chunk = Vec::<u8>::from(&binary[pointer..end]);
        let (asm_instruction, bytes_consumed) = decode(&current_chunk).unwrap();
        match simulate_line(&mut result, &asm_instruction) {
            Ok(()) => (),
            Err(e) => return Err(e),
        }
        pointer += usize::from(bytes_consumed);
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
            let new_val = current_val - val;
            state.register.insert_by_reg_name(&register, new_val);
            state.flags.set_flag('z', new_val == 0);
        }
        Ok(Assembly::Sub(register, RegisterOrValue::Value(val))) => {
            let current_val = state.register.get_by_reg_name(&register).unwrap_or(0);
            let new_val = current_val - val;
            state.register.insert_by_reg_name(&register, new_val);
            state.flags.set_flag('z', new_val == 0);
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
        "bits" => Ok(Assembly::Bit),
        _ => {
            println!("Parse error for {:?}", command);
            Err(ParseAssemblyError::Unknown)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use crate::simulator::{flags::Flags, simulate_line, Cpu, ParseAssemblyError};

    use super::{simulate_from_binary, simulate_from_code};
    #[test]
    fn simulate_simple_mov() {
        let code = "mov ax, 3";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
    }

    #[test]
    fn simulate_multiline_mov() {
        let code = "mov ax, 3
      mov bx, 4";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 4);
    }

    #[test]
    fn simulate_multiline_mov_with_comment() {
        let code = "mov ax, 3 ;comment
      mov bx, 4";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 4);
    }

    #[test]
    fn simulate_mov_register() {
        let code = "mov ax, 3
      mov bx, ax";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 3);
    }

    #[test]
    fn simulate_multiline_mov_with_blank_lines() {
        let code = "mov ax, 3

      mov bx, 4";
        let result = simulate_from_code(code.to_string()).unwrap();

        assert_eq!(result.register["ax"], 3);
        assert_eq!(result.register["bx"], 4);
    }

    #[test]
    fn bit_do_nothing() {
        let code = "bits 16";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
        };
        let result = simulate_line(&mut current_state, code);
        assert_eq!(result, Ok(()));
        assert_eq!(current_state.register.len(), 0);
    }

    #[test]
    fn simulate_line_simple_mov() {
        let code = "mov ax, 3";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
        };
        simulate_line(&mut current_state, code).unwrap();

        assert_eq!(current_state.register["ax"], 3);
    }

    #[test]
    fn simulate_line_mov_register() {
        let code = "mov ax, bx";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
        };
        current_state.register.insert("bx".into(), 3);
        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 3);
    }

    #[test]
    fn mov_err_without_params() {
        let code = "mov";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
        };
        // let result = simue(code.to_string());
        let result = simulate_line(&mut current_state, code);

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "mov required a parameter".to_string()
            ))
        );
    }

    #[test]
    fn mov_err_without_value() {
        let code = "mov ax";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
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
    fn sub_err_without_params() {
        let code = "sub";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
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
    fn sub_err_without_value() {
        let code = "sub ax";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
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
    fn simulate_sub_from_existing_state_no_flag() {
        let code = "sub ax, 2";
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16)]),
            flags: HashSet::new(),
        };

        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 4);
        assert!(!current_state.flags.get_all_flags_sorted().contains("z"));
    }

    #[test]
    fn simulate_sub_from_existing_state_with_flag() {
        let code = "sub ax, 6";
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16)]),
            flags: HashSet::new(),
        };

        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 0);
        assert!(current_state.flags.get_all_flags_sorted().contains("z"));
    }

    #[test]
    fn simulate_sub_from_existing_state_with_flag_and_reg() {
        let code = "sub ax, bx";
        let mut current_state = Cpu {
            register: HashMap::from_iter([("ax".to_string(), 6u16), ("bx".to_string(), 6u16)]),
            flags: HashSet::new(),
        };

        simulate_line(&mut current_state, code).unwrap();
        assert_eq!(current_state.register["ax"], 0);
        assert!(current_state.flags.get_all_flags_sorted().contains("z"));
    }

    #[test]
    fn simulate_line_mov_with_existing_state() {
        let code = "mov bx, 4";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashSet::new(),
        };
        current_state.register.insert("bx".into(), 3);
        simulate_line(&mut current_state, code).unwrap();

        assert_eq!(current_state.register["bx"], 4);
    }

    #[test]
    fn test_simulate_from_binary() {
        let binary = [185, 3, 0, 184, 4, 0, 137, 195]; // From asm/test_simulation
        let result = simulate_from_binary(&binary).unwrap().register;

        assert_eq!(result["ax"], 4);
        assert_eq!(result["bx"], 4);
        assert_eq!(result["cx"], 3);
    }
}
