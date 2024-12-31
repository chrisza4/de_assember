use num_enum::TryFromPrimitive;

use crate::binary::BinaryOperator;

#[repr(u8)]
#[derive(PartialEq, Debug)]
enum RegisterDirection {
    SourceInRegField = 0,
    DestInRegField = 1,
}

#[repr(u8)]
#[derive(PartialEq, Debug)]
enum WordByteOperation {
    Byte = 0,
    Word = 1,
}

#[derive(PartialEq, Debug)]
enum OpCode {
    RmToOrFromRegister,
}

#[repr(u8)]
#[derive(PartialEq, Debug, TryFromPrimitive)]
enum MovMode {
    MemoryNoDisplacement = 0,
    Memory8Bit = 1,
    Memory16Bit = 2,
    RegisterToRegister = 3,
}

pub fn decode(instruction: &Vec<&u8>) -> Option<(String, u8)> {
    let first_byte = instruction.first();
    let opcode = decode_opcode(first_byte.unwrap());
    match opcode {
        Ok(OpCode::RmToOrFromRegister) => {
            decode_rm_toorfrom_reg(&instruction.iter().map(|x| **x).collect::<Vec<u8>>())
        }
        Err(e) => panic!("{}", e),
    }
}

// Return instruction and bytes consumed
fn decode_rm_toorfrom_reg(instruction: &Vec<u8>) -> Option<(String, u8)> {
    let opcode = "mov";
    let first_byte = instruction.first();
    let second_byte = instruction.get(1);
    let third_byte = instruction.get(2);
    let fourth_byte = instruction.get(3);
    match (first_byte, second_byte) {
        (Some(first_byte), Some(second_byte)) => {
            let reg = decode_reg_field(first_byte, second_byte).to_lowercase();
            let rm_result = decode_rm_field(first_byte, second_byte, third_byte, fourth_byte);
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

fn decode_mov_mode(second_byte: &u8) -> Option<MovMode> {
    match MovMode::try_from_primitive(second_byte >> 6) {
        Ok(e) => Some(e),
        Err(_) => None,
    }
}

fn decode_opcode(first_byte: &u8) -> Result<OpCode, String> {
    const RM_TO_FROM_REGISTER: u8 = 0b100010;
    if first_byte.binary_starts_with(RM_TO_FROM_REGISTER) {
        return Ok(OpCode::RmToOrFromRegister);
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
    first_byte: &u8,
    second_byte: &u8,
    third_byte: Option<&u8>,
    fourth_byte: Option<&u8>,
) -> (String, u8) {
    let register_bits = second_byte & 0b00000111;
    let word_byte_operation = decode_wordbyte_operation(first_byte);
    let move_mode = decode_mov_mode(&second_byte);
    match move_mode {
        Some(MovMode::RegisterToRegister) => {
            (decode_register(&register_bits, word_byte_operation), 2)
        }
        Some(MovMode::Memory8Bit) => {
            let effective_address = decode_effective_address(&second_byte);
            let third_byte = third_byte.unwrap();
            (format!("[{} + {}]", effective_address, third_byte), 3)
        }
        Some(MovMode::Memory16Bit) => {
            let effective_address = decode_effective_address(&second_byte);
            let third_byte = third_byte.unwrap();
            let fourth_byte = fourth_byte.unwrap();
            let value = ((*third_byte as u16) << 8) | (*fourth_byte as u16);
            (format!("[{} + {}]", effective_address, value), 4)
        }
        Some(MovMode::MemoryNoDisplacement) => {
            let effective_address = decode_effective_address(&second_byte);
            if effective_address == "bp" {
                let third_byte = third_byte.unwrap();
                let fourth_byte = fourth_byte.unwrap();
                let value = ((*third_byte as u16) << 8) | (*fourth_byte as u16);
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
    use crate::binary::split_u16_to_u8;

    use super::*;

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
    fn simple_mod10_instruction() {
        let first = 0b10001010;
        let second = 0b10000000;
        let value: u16 = 5432;
        let (third, fourth) = split_u16_to_u8(value);
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
    fn mod00_direct_address_instruction() {
        let first = 0b10001011;
        let second = 0b00101110;
        let value: u16 = 3458;
        let (third, fourth) = split_u16_to_u8(value);
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
        let first_byte_with_byte_mode: u8 = 0b10001000;
        assert_eq!(
            "AL",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000000, None, None).0
        );
        assert_eq!(
            "CL",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000001, None, None).0
        );
        assert_eq!(
            "DL",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000010, None, None).0
        );
        assert_eq!(
            "BL",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000011, None, None).0
        );
        assert_eq!(
            "AH",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000100, None, None).0
        );
        assert_eq!(
            "CH",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000101, None, None).0
        );
        assert_eq!(
            "DH",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000110, None, None).0
        );
        assert_eq!(
            "BH",
            decode_rm_field(&first_byte_with_byte_mode, &0b11000111, None, None).0
        );

        let first_byte_with_word_mode: u8 = 0b10001001;
        assert_eq!(
            "AX",
            decode_rm_field(&first_byte_with_word_mode, &0b11000000, None, None).0
        );
        assert_eq!(
            "CX",
            decode_rm_field(&first_byte_with_word_mode, &0b11000001, None, None).0
        );
        assert_eq!(
            "DX",
            decode_rm_field(&first_byte_with_word_mode, &0b11000010, None, None).0
        );
        assert_eq!(
            "BX",
            decode_rm_field(&first_byte_with_word_mode, &0b11000011, None, None).0
        );
        assert_eq!(
            "SP",
            decode_rm_field(&first_byte_with_word_mode, &0b11000100, None, None).0
        );
        assert_eq!(
            "BP",
            decode_rm_field(&first_byte_with_word_mode, &0b11000101, None, None).0
        );
        assert_eq!(
            "SI",
            decode_rm_field(&first_byte_with_word_mode, &0b11000110, None, None).0
        );
        assert_eq!(
            "DI",
            decode_rm_field(&first_byte_with_word_mode, &0b11000111, None, None).0
        );
    }
}
