use num_enum::TryFromPrimitive;

use binary::{combined_u8, BinaryOperator};

use super::binary;

#[repr(u8)]
#[derive(PartialEq, Debug)]
enum RegisterDirection {
    SourceInRegField = 0,
    DestInRegField = 1,
}

#[repr(u8)]
#[derive(PartialEq, Debug, Clone, Copy)]
enum WordByteOperation {
    Byte = 0,
    Word = 1,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum OpCode {
    RmToOrFromRegister,
    ImmediateToRegister,
    ImmediateToRegisterOrMemory,
    MemoryToAccumulator,
    AccumulatorToMemory,
}

#[repr(u8)]
#[derive(PartialEq, Debug, TryFromPrimitive)]
enum MovMode {
    MemoryNoDisplacement = 0,
    Memory8Bit = 1,
    Memory16Bit = 2,
    RegisterToRegister = 3,
}

pub fn decode(instruction: &Vec<u8>) -> Option<(String, u8)> {
    let first_byte = instruction.first();
    let opcode = decode_opcode(first_byte.unwrap());
    let instruction_deref = instruction.iter().map(|x| *x).collect::<Vec<u8>>();
    match opcode {
        Ok(OpCode::RmToOrFromRegister) => decode_rm_toorfrom_reg(&instruction_deref),
        Ok(OpCode::ImmediateToRegister) => decode_immediate_to_register(&instruction_deref),
        Ok(OpCode::ImmediateToRegisterOrMemory) => {
            decode_immediate_to_register_or_memory(&instruction_deref)
        }
        Ok(OpCode::MemoryToAccumulator) => decode_mem_to_accumulator(&instruction_deref),
        Ok(OpCode::AccumulatorToMemory) => decode_accumulator_to_mem(&instruction_deref),
        Err(e) => panic!("{}", e),
    }
}

fn decode_immediate_to_register_or_memory(instruction: &[u8]) -> Option<(String, u8)> {
    const OPCODE: &str = "mov";
    let first_byte = *instruction.first().unwrap();
    let second_byte = instruction.get(1).unwrap();
    let third_byte = instruction.get(2);
    let fourth_byte = instruction.get(3);
    let word_byte_operation = if (first_byte & 0b00000001) == 0b00000001 {
        WordByteOperation::Word
    } else {
        WordByteOperation::Byte
    };
    let (register, bit_consumed_on_rm) =
        decode_rm_field(word_byte_operation, second_byte, third_byte, fourth_byte);
    let data1 = instruction.get(bit_consumed_on_rm as usize);
    let data2 = instruction.get((bit_consumed_on_rm as usize) + 1);

    let (value, bit_consumed_on_data) = if word_byte_operation == WordByteOperation::Word {
        (combined_u8(*data2.unwrap(), *data1.unwrap()), 2)
    } else {
        (*data1.unwrap() as u16, 1)
    };

    let identifier = if word_byte_operation == WordByteOperation::Word {
        "word"
    } else {
        "byte"
    };

    Some((
        format!("{} {}, {} {}", OPCODE, register, identifier, value),
        bit_consumed_on_data + bit_consumed_on_rm,
    ))
}

fn decode_immediate_to_register(instruction: &[u8]) -> Option<(String, u8)> {
    let opcode = "mov";
    let first_byte = *instruction.first().unwrap();
    let register_bits = first_byte % 8;
    let (word_byte_operation, bit_consumed) = if (first_byte & 0b00001000) == 0b00001000 {
        (WordByteOperation::Word, 3u8)
    } else {
        (WordByteOperation::Byte, 2u8)
    };

    let second_byte = *instruction.get(1).unwrap();
    let immediate: u16 = if word_byte_operation == WordByteOperation::Byte {
        second_byte.into()
    } else {
        let third_byte = *instruction.get(2).unwrap();
        combined_u8(third_byte, second_byte)
    };
    let reg = decode_register(&register_bits, word_byte_operation);
    let result = format!("{} {}, {}", opcode, reg.to_lowercase(), immediate);
    Some((result, bit_consumed))
}

// Return instruction and bytes consumed
fn decode_rm_toorfrom_reg(instruction: &[u8]) -> Option<(String, u8)> {
    let opcode = "mov";
    let first_byte = instruction.first();
    let second_byte = instruction.get(1);
    let third_byte = instruction.get(2);
    let fourth_byte = instruction.get(3);
    match (first_byte, second_byte) {
        (Some(first_byte), Some(second_byte)) => {
            let reg = decode_reg_field(first_byte, second_byte).to_lowercase();
            let word_byte_operation = decode_wordbyte_operation(first_byte);
            let rm_result =
                decode_rm_field(word_byte_operation, second_byte, third_byte, fourth_byte);
            let rm = rm_result.0.to_lowercase();
            let bytes_consumed = rm_result.1;
            let direction = decode_register_direction(first_byte);
            if direction == RegisterDirection::SourceInRegField {
                let result = format!("{} {}, {}", opcode, rm, reg);
                Some((result, bytes_consumed))
            } else {
                let result = format!("{} {}, {}", opcode, reg, rm);
                Some((result, bytes_consumed))
            }
        }
        _ => None,
    }
}

fn decode_mem_to_accumulator(instruction: &[u8]) -> Option<(String, u8)> {
    let first_byte = instruction.first().unwrap();
    let second_byte = instruction.get(1);
    let third_byte = instruction.get(2);

    let word_byte_operation = if (first_byte & 1) == 0 {
        WordByteOperation::Byte
    } else {
        WordByteOperation::Word
    };

    let address = if word_byte_operation == WordByteOperation::Word {
        combined_u8(*third_byte.unwrap(), *second_byte.unwrap())
    } else {
        *second_byte.unwrap() as u16
    };

    Some((format!("mov ax, [{}]", address), 3))
}

fn decode_accumulator_to_mem(instruction: &[u8]) -> Option<(String, u8)> {
    let first_byte = instruction.first().unwrap();
    let second_byte = instruction.get(1);
    let third_byte = instruction.get(2);

    let word_byte_operation = if (first_byte & 1) == 0 {
        WordByteOperation::Byte
    } else {
        WordByteOperation::Word
    };

    let address = if word_byte_operation == WordByteOperation::Word {
        combined_u8(*third_byte.unwrap(), *second_byte.unwrap())
    } else {
        *second_byte.unwrap() as u16
    };

    Some((format!("mov [{}], ax", address), 3))
}

fn decode_mov_mode(second_byte: &u8) -> Option<MovMode> {
    match MovMode::try_from_primitive(second_byte >> 6) {
        Ok(e) => Some(e),
        Err(_) => None,
    }
}

fn decode_opcode(first_byte: &u8) -> Result<OpCode, String> {
    const RM_TO_FROM_REGISTER: u8 = 0b100010;
    const IMMEDIATE_TO_REGISTER: u8 = 0b1011;
    const IMMEDIATE_TO_REGISTER_OR_MEMORY: u8 = 0b1100011;
    const MEMORY_TO_ACCUMULATOR: u8 = 0b1010000;
    const ACCUMULATOR_TO_MEMORY: u8 = 0b1010001;
    if first_byte.binary_starts_with(RM_TO_FROM_REGISTER) {
        return Ok(OpCode::RmToOrFromRegister);
    } else if first_byte.binary_starts_with(IMMEDIATE_TO_REGISTER) {
        return Ok(OpCode::ImmediateToRegister);
    } else if first_byte.binary_starts_with(IMMEDIATE_TO_REGISTER_OR_MEMORY) {
        return Ok(OpCode::ImmediateToRegisterOrMemory);
    } else if first_byte.binary_starts_with(MEMORY_TO_ACCUMULATOR) {
        return Ok(OpCode::MemoryToAccumulator);
    } else if first_byte.binary_starts_with(ACCUMULATOR_TO_MEMORY) {
        return Ok(OpCode::AccumulatorToMemory);
    }
    Err("Invalid Opcode".to_string())
}

fn decode_register_direction(first_byte: &u8) -> RegisterDirection {
    let direction_bit = 0b10;
    if (direction_bit & first_byte) == 0 {
        RegisterDirection::SourceInRegField
    } else {
        RegisterDirection::DestInRegField
    }
}

fn decode_wordbyte_operation(first_byte: &u8) -> WordByteOperation {
    let wordbyte_bit = 0b1;
    if (wordbyte_bit & first_byte) == 0 {
        WordByteOperation::Byte
    } else {
        WordByteOperation::Word
    }
}

fn decode_reg_field(first_byte: &u8, second_byte: &u8) -> String {
    let register_bits = (second_byte & 0b00111000) >> 3;
    let word_byte_operation = decode_wordbyte_operation(first_byte);
    decode_register(&register_bits, word_byte_operation)
}

fn decode_rm_field(
    word_byte_operation: WordByteOperation,
    second_byte: &u8,
    third_byte: Option<&u8>,
    fourth_byte: Option<&u8>,
) -> (String, u8) {
    let register_bits = second_byte & 0b00000111;
    let move_mode = decode_mov_mode(second_byte);
    match move_mode {
        Some(MovMode::RegisterToRegister) => {
            (decode_register(&register_bits, word_byte_operation), 2)
        }
        Some(MovMode::Memory8Bit) => {
            let effective_address = decode_effective_address(second_byte);
            let third_byte = *third_byte.unwrap() as i8;
            let sign = if third_byte >= 0 { '+' } else { '-' };
            (
                format!("[{} {} {}]", effective_address, sign, third_byte.abs()),
                3,
            )
        }
        Some(MovMode::Memory16Bit) => {
            let effective_address = decode_effective_address(second_byte);
            let third_byte = third_byte.unwrap();
            let fourth_byte = fourth_byte.unwrap();
            let value = combined_u8(*fourth_byte, *third_byte);
            (format!("[{} + {}]", effective_address, value), 4)
        }
        Some(MovMode::MemoryNoDisplacement) => {
            let effective_address = decode_effective_address(second_byte);
            if effective_address == "bp" {
                let third_byte = third_byte.unwrap();
                let fourth_byte = fourth_byte.unwrap();
                let value = combined_u8(*fourth_byte, *third_byte);
                return (format!("[{}]", value), 4);
            }
            (format!("[{}]", effective_address), 2)
        }
        _ => panic!("Unsupported mov mode"),
    }
}

fn decode_effective_address(second_byte: &u8) -> String {
    let rm_bits = second_byte % 8;
    match rm_bits {
        0b000 => "bx + si",
        0b001 => "bx + di",
        0b010 => "bp + si",
        0b011 => "bp + di",
        0b100 => "si",
        0b101 => "di",
        0b110 => "bp",
        0b111 => "bx",
        _ => "",
    }
    .to_string()
}

fn decode_register(register_bits: &u8, word_byte_operation: WordByteOperation) -> String {
    match (word_byte_operation, register_bits) {
        (WordByteOperation::Byte, 0b000) => String::from("AL"),
        (WordByteOperation::Byte, 0b001) => String::from("CL"),
        (WordByteOperation::Byte, 0b010) => String::from("DL"),
        (WordByteOperation::Byte, 0b011) => String::from("BL"),
        (WordByteOperation::Byte, 0b100) => String::from("AH"),
        (WordByteOperation::Byte, 0b101) => String::from("CH"),
        (WordByteOperation::Byte, 0b110) => String::from("DH"),
        (WordByteOperation::Byte, 0b111) => String::from("BH"),

        (WordByteOperation::Word, 0b000) => String::from("AX"),
        (WordByteOperation::Word, 0b001) => String::from("CX"),
        (WordByteOperation::Word, 0b010) => String::from("DX"),
        (WordByteOperation::Word, 0b011) => String::from("BX"),
        (WordByteOperation::Word, 0b100) => String::from("SP"),
        (WordByteOperation::Word, 0b101) => String::from("BP"),
        (WordByteOperation::Word, 0b110) => String::from("SI"),
        (WordByteOperation::Word, 0b111) => String::from("DI"),
        _ => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::binary::*;
    use super::*;

    #[test]
    fn simple_immeidate_to_register() {
        let encoded_instruction = [177u8, 12, 181, 244];
        let decoded_instruction = decode(&encoded_instruction.into()).unwrap();

        assert_eq!("mov cl, 12", decoded_instruction.0);
        assert_eq!(2, decoded_instruction.1);
    }
    #[test]
    fn simple_mod11_instruction() {
        let encoded_instruction = vec![137u8, 217];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();

        assert_eq!("mov cx, bx", decoded_instruction.0);
        assert_eq!(2, decoded_instruction.1);
    }

    #[test]
    fn simple_mod01_instruction() {
        let first = 0b10001010;
        let second = 0b01000000;
        let third: u8 = 20;
        let encoded_instruction = vec![first, second, third];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();
        assert_eq!("mov al, [bx + si + 20]", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);

        let first = 0b10001010;
        let second = 0b01000001;
        let third: u8 = 20;
        let encoded_instruction = vec![first, second, third];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();
        assert_eq!("mov al, [bx + di + 20]", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn simple_mod01_memory_destination_instruction() {
        let first = 0b10001000;
        let second = 0b01000000;
        let third: u8 = 20;
        let encoded_instruction = vec![first, second, third];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();
        assert_eq!("mov [bx + si + 20], al", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn simple_mod01_minus_value() {
        let first = 0b10001000;
        let second = 0b01000000;
        let third: u8 = (-1i8) as u8;
        let encoded_instruction = vec![first, second, third];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();
        assert_eq!("mov [bx + si - 1], al", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn simple_mod10_instruction() {
        let first = 0b10001010;
        let second = 0b10000000;
        let value: u16 = 5432;
        let (fourth, third) = split_u16_to_u8(value);
        let encoded_instruction = vec![first, second, third, fourth];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();
        assert_eq!("mov al, [bx + si + 5432]", decoded_instruction.0);
        assert_eq!(4, decoded_instruction.1);
    }

    #[test]
    fn simple_mod00_instruction() {
        let first = 0b10001010;
        let second = 0b00000000;
        let encoded_instruction = vec![first, second];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();
        assert_eq!("mov al, [bx + si]", decoded_instruction.0);
        assert_eq!(2, decoded_instruction.1);
    }

    #[test]
    fn immediate_to_register_instruction_byte() {
        let encoded_instruction = [198u8, 3, 7];
        let decoded_instruction = decode(&encoded_instruction.into()).unwrap();
        assert_eq!("mov [bp + di], byte 7", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn immediate_to_register_instruction_word() {
        let encoded_instruction = [199, 133, 133, 3, 91, 1];
        let decoded_instruction = decode(&encoded_instruction.into()).unwrap();
        assert_eq!("mov [di + 901], word 347", decoded_instruction.0);
        assert_eq!(6, decoded_instruction.1);
    }

    #[test]
    fn test_memory_to_accumulator() {
        let encoded_instruction = [161, 251, 9];
        let decoded_instruction = decode(&encoded_instruction.into()).unwrap();
        assert_eq!("mov ax, [2555]", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);

        let encoded_instruction = [161, 16, 0];
        let decoded_instruction = decode(&encoded_instruction.into()).unwrap();
        assert_eq!("mov ax, [16]", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn test_accumulator_to_memory() {
        let encoded_instruction = [163, 15, 0];
        let decoded_instruction = decode(&encoded_instruction.into()).unwrap();
        assert_eq!("mov [15], ax", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);

        let encoded_instruction = [163, 250, 9];
        let decoded_instruction = decode(&encoded_instruction.into()).unwrap();
        assert_eq!("mov [2554], ax", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn mod00_direct_address_instruction() {
        let first = 0b10001011;
        let second = 0b00101110;
        let value: u16 = 3458;
        let (fourth, third) = split_u16_to_u8(value);
        let encoded_instruction = vec![first, second, third, fourth];
        let decoded_instruction = decode_rm_toorfrom_reg(&encoded_instruction).unwrap();

        assert_eq!("mov bp, [3458]", decoded_instruction.0);
        assert_eq!(4, decoded_instruction.1);
    }

    #[test]
    fn test_decode_opcode() {
        assert_eq!(Ok(OpCode::RmToOrFromRegister), decode_opcode(&0b10001001));
        assert_eq!(Ok(OpCode::RmToOrFromRegister), decode_opcode(&0b10001011));
        assert_eq!(Ok(OpCode::RmToOrFromRegister), decode_opcode(&0b10001010));
    }

    #[test]
    fn test_decode_register_direction() {
        assert_eq!(
            RegisterDirection::SourceInRegField,
            decode_register_direction(&0b10001000)
        );
        assert_eq!(
            RegisterDirection::DestInRegField,
            decode_register_direction(&0b10001010)
        );
    }

    #[test]
    fn test_decode_wordbyte_operation() {
        assert_eq!(
            WordByteOperation::Byte,
            decode_wordbyte_operation(&0b10001000)
        );
        assert_eq!(
            WordByteOperation::Word,
            decode_wordbyte_operation(&0b10001011)
        );
    }

    #[test]
    fn test_decode_firstregister() {
        // Case byte
        let first_byte_with_byte_mode: u8 = 0b10001000;
        assert_eq!(
            "AL",
            decode_reg_field(&first_byte_with_byte_mode, &0b00000000)
        );
        assert_eq!(
            "CL",
            decode_reg_field(&first_byte_with_byte_mode, &0b00001000)
        );
        assert_eq!(
            "DL",
            decode_reg_field(&first_byte_with_byte_mode, &0b00010000)
        );
        assert_eq!(
            "BL",
            decode_reg_field(&first_byte_with_byte_mode, &0b00011000)
        );
        assert_eq!(
            "AH",
            decode_reg_field(&first_byte_with_byte_mode, &0b00100000)
        );
        assert_eq!(
            "CH",
            decode_reg_field(&first_byte_with_byte_mode, &0b00101000)
        );
        assert_eq!(
            "DH",
            decode_reg_field(&first_byte_with_byte_mode, &0b00110000)
        );
        assert_eq!(
            "BH",
            decode_reg_field(&first_byte_with_byte_mode, &0b00111000)
        );

        let first_byte_with_word_mode: u8 = 0b10001001;
        assert_eq!(
            "AX",
            decode_reg_field(&first_byte_with_word_mode, &0b00000000)
        );
        assert_eq!(
            "CX",
            decode_reg_field(&first_byte_with_word_mode, &0b00001000)
        );
        assert_eq!(
            "DX",
            decode_reg_field(&first_byte_with_word_mode, &0b00010000)
        );
        assert_eq!(
            "BX",
            decode_reg_field(&first_byte_with_word_mode, &0b00011000)
        );
        assert_eq!(
            "SP",
            decode_reg_field(&first_byte_with_word_mode, &0b00100000)
        );
        assert_eq!(
            "BP",
            decode_reg_field(&first_byte_with_word_mode, &0b00101000)
        );
        assert_eq!(
            "SI",
            decode_reg_field(&first_byte_with_word_mode, &0b00110000)
        );
        assert_eq!(
            "DI",
            decode_reg_field(&first_byte_with_word_mode, &0b00111000)
        );
    }

    #[test]
    fn test_decode_second_register() {
        // Case byte
        assert_eq!(
            "AL",
            decode_rm_field(WordByteOperation::Byte, &0b11000000, None, None).0
        );
        assert_eq!(
            "CL",
            decode_rm_field(WordByteOperation::Byte, &0b11000001, None, None).0
        );
        assert_eq!(
            "DL",
            decode_rm_field(WordByteOperation::Byte, &0b11000010, None, None).0
        );
        assert_eq!(
            "BL",
            decode_rm_field(WordByteOperation::Byte, &0b11000011, None, None).0
        );
        assert_eq!(
            "AH",
            decode_rm_field(WordByteOperation::Byte, &0b11000100, None, None).0
        );
        assert_eq!(
            "CH",
            decode_rm_field(WordByteOperation::Byte, &0b11000101, None, None).0
        );
        assert_eq!(
            "DH",
            decode_rm_field(WordByteOperation::Byte, &0b11000110, None, None).0
        );
        assert_eq!(
            "BH",
            decode_rm_field(WordByteOperation::Byte, &0b11000111, None, None).0
        );

        assert_eq!(
            "AX",
            decode_rm_field(WordByteOperation::Word, &0b11000000, None, None).0
        );
        assert_eq!(
            "CX",
            decode_rm_field(WordByteOperation::Word, &0b11000001, None, None).0
        );
        assert_eq!(
            "DX",
            decode_rm_field(WordByteOperation::Word, &0b11000010, None, None).0
        );
        assert_eq!(
            "BX",
            decode_rm_field(WordByteOperation::Word, &0b11000011, None, None).0
        );
        assert_eq!(
            "SP",
            decode_rm_field(WordByteOperation::Word, &0b11000100, None, None).0
        );
        assert_eq!(
            "BP",
            decode_rm_field(WordByteOperation::Word, &0b11000101, None, None).0
        );
        assert_eq!(
            "SI",
            decode_rm_field(WordByteOperation::Word, &0b11000110, None, None).0
        );
        assert_eq!(
            "DI",
            decode_rm_field(WordByteOperation::Word, &0b11000111, None, None).0
        );
    }
}
