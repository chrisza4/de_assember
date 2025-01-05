use std::collections::HashMap;

pub fn simulate(code: String) -> Result<HashMap<String, u16>, ParseAssemblyError> {
    let mut result = HashMap::<String, u16>::new();
    let lines = code.split('\n');
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        let assembly = parse_assembly_code(line);
        match assembly {
            Ok(Assembly::Mov(register, RegisterOrValue::Value(val))) => {
                result.insert(register.to_string(), val);
            }
            Ok(Assembly::Mov(register, RegisterOrValue::Register(from_reg))) => {
                let val = result.get(&from_reg).unwrap();

                result.insert(register.to_string(), *val);
            }
            Ok(Assembly::Bit) => (),
            Err(e) => return Err(e),
        }
    }
    Ok(result)
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

fn parse_assembly_code(code: &str) -> Result<Assembly, ParseAssemblyError> {
    let command_and_params = code.trim().split_once(' ');
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
        "bit" => Ok(Assembly::Bit),
        _ => {
            println!("Parse error for {:?}", command);
            Err(ParseAssemblyError::Unknown)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::simulator::ParseAssemblyError;

    use super::simulate;

    #[test]
    fn simulate_simple_mov() {
        let code = "mov ax, 3";
        let result = simulate(code.to_string()).unwrap();

        assert_eq!(result["ax"], 3);
    }

    #[test]
    fn simulate_multiline_mov() {
        let code = "mov ax, 3
      mov bx, 4";
        let result = simulate(code.to_string()).unwrap();

        assert_eq!(result["ax"], 3);
        assert_eq!(result["bx"], 4);
    }

    #[test]
    fn simulate_mov_register() {
        let code = "mov ax, 3
      mov bx, ax";
        let result = simulate(code.to_string()).unwrap();

        assert_eq!(result["ax"], 3);
        assert_eq!(result["bx"], 3);
    }

    #[test]
    fn simulate_multiline_mov_with_blank_lines() {
        let code = "mov ax, 3

      mov bx, 4";
        let result = simulate(code.to_string()).unwrap();

        assert_eq!(result["ax"], 3);
        assert_eq!(result["bx"], 4);
    }

    #[test]
    fn mov_err_without_params() {
        let code = "mov";
        let result = simulate(code.to_string());

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
        let result = simulate(code.to_string());

        assert_eq!(
            result,
            Err(ParseAssemblyError::InvalidParams(
                "Mov required 2 parameters".to_string()
            ))
        );
    }

    #[test]
    fn bit_do_nothing() {
        let code = "bit 16";
        let result = simulate(code.to_string()).unwrap();
        assert_eq!(result.len(), 0);
    }
}
