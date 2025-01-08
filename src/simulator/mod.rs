use register_set::RegisterSet;

use crate::deassembler::decoder::decode;
use std::{cmp::min, collections::HashMap};

mod register_set;
mod flags;

pub struct Cpu {
    pub register: HashMap<String, u16>,
    pub flags: HashMap<char, bool>
}

#[allow(dead_code)]
pub fn simulate_from_code(code: String) -> Result<Cpu, ParseAssemblyError> {
    let mut result = Cpu {
        register: HashMap::<String, u16>::new(),
        flags: HashMap::new(),
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
        flags: HashMap::new(),
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

pub fn simulate_line(
    state: &mut Cpu,
    line: &str,
) -> Result<(), ParseAssemblyError> {
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

fn parse_assembly_code(code: &str) -> Result<Assembly, ParseAssemblyError> {
    let code_without_comment = strip_comment(code);
    let command_and_params = code_without_comment.trim().split_once(' ');
    let command = command_and_params.map(|x| x.0).unwrap_or(code);
    match command {
        "mov" => {
            let Some(params) = command_and_params.map(|x| x.1) else {
                return Err(ParseAssemblyError::InvalidParams(
                    "Mov required a parameter".to_string(),
                ));
            };
            let move_params: Vec<&str> = params.split(',').collect();
            let register_to_mov_to = move_params.first().unwrap();
            let Some(value) = move_params.get(1).map(|x| x.trim()) else {
                return Err(ParseAssemblyError::InvalidParams(
                    "Mov required 2 parameters".to_string(),
                ));
            };
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
        "bits" => Ok(Assembly::Bit),
        _ => {
            println!("Parse error for {:?}", command);
            Err(ParseAssemblyError::Unknown)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::simulator::{simulate_line, Cpu, ParseAssemblyError};

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
            flags: HashMap::new()
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
            flags: HashMap::new()
        };
        simulate_line(&mut current_state, code).unwrap();

        assert_eq!(current_state.register["ax"], 3);
    }

    #[test]
    fn simulate_line_mov_register() {
        let code = "mov ax, bx";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashMap::new()
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
            flags: HashMap::new()
        };
        // let result = simue(code.to_string());
        let result = simulate_line(&mut current_state, code);

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "Mov required a parameter".to_string()
            ))
        );
    }

    #[test]
    fn mov_err_without_value() {
        let code = "mov ax";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashMap::new()
        };
        let result = simulate_line(&mut current_state, code);

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "Mov required 2 parameters".to_string()
            ))
        );
    }

    #[test]
    fn simulate_line_mov_with_existing_state() {
        let code = "mov bx, 4";
        let mut current_state = Cpu {
            register: HashMap::new(),
            flags: HashMap::new()
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
