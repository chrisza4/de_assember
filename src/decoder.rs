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

pub fn decode(instruction: &[u8]) -> Option<String> {
    let first_byte = instruction.first();
    let second_byte = instruction.get(1);
    match (first_byte, second_byte) {
        (Some(first_byte), Some(second_byte)) => {
            let opcode = decode_opcode(first_byte);
            let reg = decode_reg_field(first_byte, second_byte).to_lowercase();
            let rm = decode_rm_field(first_byte, second_byte).to_lowercase();
            let direction = decode_register_direction(first_byte);
            if direction == RegisterDirection::SourceInRegField {
                let result = format!("{} {}, {}", opcode, rm, reg);
                Some(result)
            } else {
                let result = format!("{} {}, {}", opcode, reg, rm);
                Some(result)
            }
        }
        _ => None,
    }
}

fn decode_opcode(first_byte: &u8) -> String {
    let command = first_byte >> 2;
    let mov_command: u8 = 0b100010;
    if command == mov_command {
        return String::from("mov");
    }
    String::new()
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

fn decode_rm_field(first_byte: &u8, second_byte: &u8) -> String {
    let register_bits = second_byte & 0b00000111;
    let word_byte_operation = decode_wordbyte_operation(first_byte);
    decode_register(&register_bits, word_byte_operation)
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
    use super::*;

    #[test]
    fn simple_instruction() {
        let encoded_instruction = vec![137u8, 217];
        let decoded_instruction = decode(&encoded_instruction);
        assert_eq!(Some("mov cx, bx"), decoded_instruction.as_deref())
    }

    #[test]
    fn test_decode_opcode() {
        assert_eq!("mov", decode_opcode(&0b10001001));
        assert_eq!("mov", decode_opcode(&0b10001011));
        assert_eq!("mov", decode_opcode(&0b10001010));
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
            decode_rm_field(&first_byte_with_byte_mode, &0b00000000)
        );
        assert_eq!(
            "CL",
            decode_rm_field(&first_byte_with_byte_mode, &0b00000001)
        );
        assert_eq!(
            "DL",
            decode_rm_field(&first_byte_with_byte_mode, &0b00000010)
        );
        assert_eq!(
            "BL",
            decode_rm_field(&first_byte_with_byte_mode, &0b00000011)
        );
        assert_eq!(
            "AH",
            decode_rm_field(&first_byte_with_byte_mode, &0b00000100)
        );
        assert_eq!(
            "CH",
            decode_rm_field(&first_byte_with_byte_mode, &0b00000101)
        );
        assert_eq!(
            "DH",
            decode_rm_field(&first_byte_with_byte_mode, &0b00000110)
        );
        assert_eq!(
            "BH",
            decode_rm_field(&first_byte_with_byte_mode, &0b00000111)
        );

        let first_byte_with_word_mode: u8 = 0b10001001;
        assert_eq!(
            "AX",
            decode_rm_field(&first_byte_with_word_mode, &0b00000000)
        );
        assert_eq!(
            "CX",
            decode_rm_field(&first_byte_with_word_mode, &0b00000001)
        );
        assert_eq!(
            "DX",
            decode_rm_field(&first_byte_with_word_mode, &0b00000010)
        );
        assert_eq!(
            "BX",
            decode_rm_field(&first_byte_with_word_mode, &0b00000011)
        );
        assert_eq!(
            "SP",
            decode_rm_field(&first_byte_with_word_mode, &0b00000100)
        );
        assert_eq!(
            "BP",
            decode_rm_field(&first_byte_with_word_mode, &0b00000101)
        );
        assert_eq!(
            "SI",
            decode_rm_field(&first_byte_with_word_mode, &0b00000110)
        );
        assert_eq!(
            "DI",
            decode_rm_field(&first_byte_with_word_mode, &0b00000111)
        );
    }
}
