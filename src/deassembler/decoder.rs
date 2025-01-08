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
    RmToSegmentRegister,
    SegmentRegisterToRm,
    SubImmediateFromRm,
    SubImmediateFromAccumulator,
    SubRmAndRegisterToEither,
    AddRmAndRegisterToEither,
    AddImmediateFromRm,
    AddImmediateFromAccumulator,
    CmpImmediateFromRm,
    CmpRmAndRegisterToEither,
    CmpImmediateFromAccumulator,
}

#[repr(u8)]
#[derive(PartialEq, Debug, TryFromPrimitive)]
enum MovMode {
    MemoryNoDisplacement = 0,
    Memory8Bit = 1,
    Memory16Bit = 2,
    RegisterToRegister = 3,
}

pub fn decode(instruction: &[u8]) -> Option<(String, u8)> {
    let first_byte = instruction.first();
    let second_byte = instruction.get(1);
    let opcode = decode_opcode(first_byte.unwrap(), second_byte);
    let instruction_deref = instruction.to_vec();
    match opcode {
        Ok(OpCode::RmToOrFromRegister) => decode_rm_toorfrom_reg(&instruction_deref),
        Ok(OpCode::ImmediateToRegister) => decode_mov_immediate_to_register(&instruction_deref),
        Ok(OpCode::ImmediateToRegisterOrMemory) => {
            decode_mov_immediate_to_register_or_memory(&instruction_deref)
        }
        Ok(OpCode::MemoryToAccumulator) => decode_mov_mem_to_accumulator(&instruction_deref),
        Ok(OpCode::AccumulatorToMemory) => decode_accumulator_to_mem(&instruction_deref),
        Ok(OpCode::RmToSegmentRegister) => decode_rm_to_segment(&instruction_deref),
        Ok(OpCode::SegmentRegisterToRm) => decode_segment_to_rm(&instruction_deref),
        Ok(OpCode::SubImmediateFromRm) => decode_sub_immediate_from_rm(&instruction_deref),
        Ok(OpCode::SubImmediateFromAccumulator) => {
            decode_sub_immediate_from_accumulator(&instruction_deref)
        }
        Ok(OpCode::SubRmAndRegisterToEither) => decode_sub_rm_and_register(&instruction_deref),
        Ok(OpCode::AddRmAndRegisterToEither) => decode_add_rm_and_register(&instruction_deref),
        Ok(OpCode::AddImmediateFromRm) => decode_add_immediate_from_rm(&instruction_deref),
        Ok(OpCode::AddImmediateFromAccumulator) => {
            decode_add_immediate_from_accumulator(&instruction_deref)
        }
        Ok(OpCode::CmpImmediateFromRm) => decode_cmp_immediate_from_rm(&instruction_deref),
        Ok(OpCode::CmpRmAndRegisterToEither) => decode_cmp_rm_and_register(&instruction_deref),
        Ok(OpCode::CmpImmediateFromAccumulator) => decode_cmp_immediate_from_accumulator(&instruction_deref),
        Err(e) => panic!("{}", e),
    }
}

fn decode_cmp_immediate_from_accumulator(instruction: &[u8]) -> Option<(String, u8)> {
    let address = decode_mem_accumulator_address(instruction);
    Some((format!("cmp ax, {}", address), 3))
}

fn decode_cmp_rm_and_register(instruction: &[u8]) -> Option<(String, u8)> {
    decode_rm_register("cmp", instruction)
}

fn decode_cmp_immediate_from_rm(instruction: &[u8]) -> Option<(String, u8)> {
    const OPCODE: &str = "cmp";
    let first_byte = *instruction.first().unwrap();
    let s_bit = (first_byte & 0b00000010) >> 1;
    let r = decode_immediate_from_rm(instruction, s_bit == 1)?;
    Some((
        format!("{} {}, {}", OPCODE, r.register, r.value),
        r.bit_consumed,
    ))
}

fn decode_add_immediate_from_accumulator(instruction: &[u8]) -> Option<(String, u8)> {
    let address = decode_mem_accumulator_address(instruction);
    Some((format!("add ax, {}", address), 3))
}

fn decode_add_immediate_from_rm(instruction: &[u8]) -> Option<(String, u8)> {
    const OPCODE: &str = "add";
    let first_byte = *instruction.first().unwrap();
    let s_bit = (first_byte & 0b00000010) >> 1;
    let r = decode_immediate_from_rm(instruction, s_bit == 1)?;
    Some((
        format!("{} {}, {}", OPCODE, r.register, r.value),
        r.bit_consumed,
    ))
}

fn decode_add_rm_and_register(instruction: &[u8]) -> Option<(String, u8)> {
    decode_rm_register("add", instruction)
}

fn decode_rm_register(opcode: &str, instruction: &[u8]) -> Option<(String, u8)> {
    let first_byte = instruction.first();
    let second_byte = instruction.get(1);
    let third_byte = instruction.get(2);
    let fourth_byte = instruction.get(3);
    match (first_byte, second_byte) {
        (Some(first_byte), Some(second_byte)) => {
            let reg = decode_reg_field(first_byte, second_byte);
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

fn decode_sub_rm_and_register(instruction: &[u8]) -> Option<(String, u8)> {
    decode_rm_register("sub", instruction)
}

fn decode_sub_immediate_from_accumulator(instruction: &[u8]) -> Option<(String, u8)> {
    let address = decode_mem_accumulator_address(instruction);
    Some((format!("sub ax, {}", address), 3))
}

fn decode_sub_immediate_from_rm(instruction: &[u8]) -> Option<(String, u8)> {
    const OPCODE: &str = "sub";
    let first_byte = *instruction.first().unwrap();
    let s_bit = (first_byte & 0b00000010) >> 1;
    let r = decode_immediate_from_rm(instruction, s_bit == 1)?;
    Some((
        format!("{} {}, {}", OPCODE, r.register, r.value),
        r.bit_consumed,
    ))
}

fn decode_segment_to_rm(instruction: &[u8]) -> Option<(String, u8)> {
    let second_byte = instruction.get(1).unwrap();
    let third_byte = instruction.get(2);
    let fourth_byte = instruction.get(3);
    const SR_TABLES: [&str; 4] = ["es", "cs", "ss", "ds"];
    let (rm, bytes_consumed) = decode_rm_field(
        WordByteOperation::Word,
        second_byte,
        third_byte,
        fourth_byte,
    );
    let sr_bits = (second_byte & 0b0011000) >> 3;
    let sr = SR_TABLES[sr_bits as usize];
    Some((format!("mov {}, {}", rm, sr), bytes_consumed))
}

fn decode_rm_to_segment(instruction: &[u8]) -> Option<(String, u8)> {
    let second_byte = instruction.get(1).unwrap();
    let third_byte = instruction.get(2);
    let fourth_byte = instruction.get(3);
    const SR_TABLES: [&str; 4] = ["es", "cs", "ss", "ds"];
    let (rm, bytes_consumed) = decode_rm_field(
        WordByteOperation::Word,
        second_byte,
        third_byte,
        fourth_byte,
    );
    let sr_bits = (second_byte & 0b0011000) >> 3;
    let sr = SR_TABLES[sr_bits as usize];
    Some((format!("mov {}, {}", sr, rm), bytes_consumed))
}

struct ImmediateToRegister {
    register: String,
    identifier: String,
    value: u16,
    bit_consumed: u8,
}

fn decode_immediate_from_rm(instruction: &[u8], signed_bit: bool) -> Option<ImmediateToRegister> {
    let first_byte = *instruction.first().unwrap();
    let second_byte = instruction.get(1).unwrap();
    let third_byte = instruction.get(2);
    let fourth_byte = instruction.get(3);
    let word_byte_operation = decode_wordbyte_operation(&first_byte);
    let (register, bit_consumed_on_rm) =
        decode_rm_field(word_byte_operation, second_byte, third_byte, fourth_byte);

    let data1 = instruction.get(bit_consumed_on_rm as usize);
    let data2 = instruction.get((bit_consumed_on_rm as usize) + 1);

    let (value, bit_consumed_on_data) =
        if word_byte_operation == WordByteOperation::Word && !signed_bit {
            (combined_u8(*data2.unwrap(), *data1.unwrap()), 2)
        } else {
            (*data1.unwrap() as u16, 1)
        };

    let identifier = if word_byte_operation == WordByteOperation::Word {
        "word"
    } else {
        "byte"
    };

    Some(ImmediateToRegister {
        register,
        identifier: identifier.to_string(),
        value,
        bit_consumed: bit_consumed_on_rm + bit_consumed_on_data,
    })
}

fn decode_mov_immediate_to_register_or_memory(instruction: &[u8]) -> Option<(String, u8)> {
    const OPCODE: &str = "mov";
    let r = decode_immediate_from_rm(instruction, false)?;
    Some((
        format!("{} {}, {} {}", OPCODE, r.register, r.identifier, r.value),
        r.bit_consumed,
    ))
}

fn decode_mov_immediate_to_register(instruction: &[u8]) -> Option<(String, u8)> {
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
    let result = format!("{} {}, {}", opcode, reg, immediate);
    Some((result, bit_consumed))
}

// Return instruction and bytes consumed
fn decode_rm_toorfrom_reg(instruction: &[u8]) -> Option<(String, u8)> {
    decode_rm_register("mov", instruction)
}

fn decode_mem_accumulator_address(instruction: &[u8]) -> u16 {
    let first_byte = instruction.first().unwrap();
    let second_byte = instruction.get(1);
    let third_byte = instruction.get(2);

    let word_byte_operation = if (first_byte & 1) == 0 {
        WordByteOperation::Byte
    } else {
        WordByteOperation::Word
    };

    if word_byte_operation == WordByteOperation::Word {
        combined_u8(*third_byte.unwrap(), *second_byte.unwrap())
    } else {
        *second_byte.unwrap() as u16
    }
}

fn decode_mov_mem_to_accumulator(instruction: &[u8]) -> Option<(String, u8)> {
    let address = decode_mem_accumulator_address(instruction);
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

fn decode_opcode(first_byte: &u8, second_byte: Option<&u8>) -> Result<OpCode, String> {
    const RM_TO_FROM_REGISTER: u8 = 0b100010;
    const IMMEDIATE_TO_REGISTER: u8 = 0b1011;
    const IMMEDIATE_TO_REGISTER_OR_MEMORY: u8 = 0b1100011;
    const MEMORY_TO_ACCUMULATOR: u8 = 0b1010000;
    const ACCUMULATOR_TO_MEMORY: u8 = 0b1010001;
    const RM_TO_SEGMENT: u8 = 0b10001110;
    const SEGMENT_TO_RM: u8 = 0b10001100;
    const SUB_ADD_OR_CMP_IMMEDIATE_FROM_REGISTER: u8 = 0b100000;
    const SUB_IMMEDIATE_FROM_ACCUMULATOR: u8 = 0b00101100;
    const SUB_RM_AND_REGISTER_TO_EITHER: u8 = 0b00101000;
    const ADD_IMMEDIATE_FROM_ACCUMULATOR: u8 = 0b00000100;
    const ADD_RM_AND_REGISTER_TO_EITHER: u8 = 0b00000000;
    const CMP_IMMEDIATE_FROM_ACCUMULATOR: u8 = 0b00111100;
    const CMP_RM_AND_REGISTER_TO_EITHER: u8 = 0b00111000;

    match first_byte {
        _ if first_byte.binary_starts_with(RM_TO_FROM_REGISTER) => Ok(OpCode::RmToOrFromRegister),
        _ if first_byte.binary_starts_with(IMMEDIATE_TO_REGISTER) => {
            Ok(OpCode::ImmediateToRegister)
        }
        _ if first_byte.binary_starts_with(IMMEDIATE_TO_REGISTER_OR_MEMORY) => {
            Ok(OpCode::ImmediateToRegisterOrMemory)
        }
        _ if first_byte.binary_starts_with(MEMORY_TO_ACCUMULATOR) => {
            Ok(OpCode::MemoryToAccumulator)
        }
        _ if first_byte.binary_starts_with(ACCUMULATOR_TO_MEMORY) => {
            Ok(OpCode::AccumulatorToMemory)
        }
        _ if first_byte.binary_starts_with(RM_TO_SEGMENT) => Ok(OpCode::RmToSegmentRegister),
        _ if first_byte.binary_starts_with(SEGMENT_TO_RM) => Ok(OpCode::SegmentRegisterToRm),
        _ if first_byte.binary_starts_with(SUB_ADD_OR_CMP_IMMEDIATE_FROM_REGISTER) => {
            match second_byte {
                None => Err(format!("Invalid Opcode {:?}", first_byte)),
                Some(x) => {
                    if x.binary_at_equals(3, 0)
                        && x.binary_at_equals(4, 0)
                        && x.binary_at_equals(5, 0)
                    {
                        println!("x: {x:#b}");
                        Ok(OpCode::AddImmediateFromRm)
                    } else if x.binary_at_equals(3, 1)
                        && x.binary_at_equals(4, 1)
                        && x.binary_at_equals(5, 1)
                    {
                        Ok(OpCode::CmpImmediateFromRm)
                    } else {
                        Ok(OpCode::SubImmediateFromRm)
                    }
                }
            }
        }
        _ if (first_byte ^ SUB_IMMEDIATE_FROM_ACCUMULATOR) | 1 == 1 => {
            Ok(OpCode::SubImmediateFromAccumulator)
        }
        _ if (first_byte ^ SUB_RM_AND_REGISTER_TO_EITHER) | 0b11 == 0b11 => {
            Ok(OpCode::SubRmAndRegisterToEither)
        }
        _ if (first_byte ^ ADD_RM_AND_REGISTER_TO_EITHER) | 0b11 == 0b11 => {
            Ok(OpCode::AddRmAndRegisterToEither)
        }
        _ if (first_byte ^ ADD_IMMEDIATE_FROM_ACCUMULATOR) | 1 == 1 => {
            Ok(OpCode::AddImmediateFromAccumulator)
        }
        _ if (first_byte ^ CMP_RM_AND_REGISTER_TO_EITHER) | 0b11 == 0b11 => {
            Ok(OpCode::CmpRmAndRegisterToEither)
        }
        _ if (first_byte ^ CMP_IMMEDIATE_FROM_ACCUMULATOR) | 1 == 1 => {
            Ok(OpCode::CmpImmediateFromAccumulator)
        }

        _ => Err(format!("Invalid Opcode {:?}", first_byte)), // Handle unmatched cases if necessary
    }
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
        (WordByteOperation::Byte, 0b000) => String::from("al"),
        (WordByteOperation::Byte, 0b001) => String::from("cl"),
        (WordByteOperation::Byte, 0b010) => String::from("dl"),
        (WordByteOperation::Byte, 0b011) => String::from("bl"),
        (WordByteOperation::Byte, 0b100) => String::from("ah"),
        (WordByteOperation::Byte, 0b101) => String::from("ch"),
        (WordByteOperation::Byte, 0b110) => String::from("dh"),
        (WordByteOperation::Byte, 0b111) => String::from("bh"),

        (WordByteOperation::Word, 0b000) => String::from("ax"),
        (WordByteOperation::Word, 0b001) => String::from("cx"),
        (WordByteOperation::Word, 0b010) => String::from("dx"),
        (WordByteOperation::Word, 0b011) => String::from("bx"),
        (WordByteOperation::Word, 0b100) => String::from("sp"),
        (WordByteOperation::Word, 0b101) => String::from("bp"),
        (WordByteOperation::Word, 0b110) => String::from("si"),
        (WordByteOperation::Word, 0b111) => String::from("di"),
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
        let decoded_instruction = decode(&encoded_instruction).unwrap();

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
    fn decode_register_memory_to_segment_register() {
        let encoded_instruction = [142u8, 208];
        let decoded_instruction = decode(&encoded_instruction).unwrap();

        assert_eq!("mov ss, ax", decoded_instruction.0);
        assert_eq!(2, decoded_instruction.1);
    }

    #[test]
    fn test_decode_sub_immediate_from_rm() {
        let test_cases = [
            (vec![0b10000011u8, 0b11101000, 0b00110111], "sub ax, 55", 3),
            (
                vec![0b10000001, 0b11101011, 0b00000011, 0b11011001],
                "sub bx, 55555",
                4,
            ),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_sub_rm_with_register() {
        let test_cases = [
            (vec![0b00101001, 0b11001011], "sub bx, cx", 2),
            (
                vec![0b00101010, 0b10000000, 0b11111110, 0b11111101],
                "sub al, [bx + si + 65022]",
                4,
            ),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_sub_immediate_from_accumulator() {
        let test_cases = [
            (vec![0b00101101, 0b00000101, 0b00001101], "sub ax, 3333", 3),
            (vec![0b00101101, 0b11001110, 0b11011101], "sub ax, 56782", 3),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_cmp_immediate_from_rm() {
        let test_cases = [
            (vec![0b10000011, 0b11111000, 0b00110111], "cmp ax, 55", 3),
            (
                vec![0b10000001, 0b11111011, 0b00000011, 0b11011001],
                "cmp bx, 55555",
                4,
            ),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_cmp_rm_with_register() {
        let test_cases = [
            (vec![0b00111001, 0b11001011], "cmp bx, cx", 2),
            (
                vec![0b00111010, 0b10000000, 0b11111110, 0b11111101],
                "cmp al, [bx + si + 65022]",
                4,
            ),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_cmp_immediate_from_accumulator() {
        let test_cases = [
            (vec![0b00111101, 0b00000101, 0b00001101], "cmp ax, 3333", 3),
            (vec![0b00111101, 0b11001110, 0b11011101], "cmp ax, 56782", 3),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_add_rm_with_register() {
        let test_cases = [
            (vec![0b00000001, 0b11011000], "add ax, bx", 2),
            (
                vec![0b00000010, 0b10000000, 0b11111110, 0b11111101],
                "add al, [bx + si + 65022]",
                4,
            ),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_add_immediate_to_rm() {
        let test_cases = [
            (
                vec![0b10000001, 0b11000011, 0b10110011, 0b00010101],
                "add bx, 5555",
                4,
            ),
            (
                vec![0b00000010, 0b10000000, 0b11111110, 0b11111101],
                "add al, [bx + si + 65022]",
                4,
            ),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn test_decode_add_immediate_from_accumulator() {
        let test_cases = [
            (vec![0b10000011, 0b11000000, 0b00100001], "add ax, 33", 3),
            (vec![0b00000101, 0b11001110, 0b11011101], "add ax, 56782", 3),
        ];

        for (encoded_instruction, expected, bytes_consumed) in test_cases {
            let decoded_instruction = decode(&encoded_instruction).unwrap();
            assert_eq!(expected, decoded_instruction.0);
            assert_eq!(bytes_consumed, decoded_instruction.1);
        }
    }

    #[test]
    fn decode_segment_register_to_rm() {
        let encoded_instruction = [140u8, 212];
        let decoded_instruction = decode(&encoded_instruction).unwrap();

        assert_eq!("mov sp, ss", decoded_instruction.0);
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
        let decoded_instruction = decode(&encoded_instruction).unwrap();
        assert_eq!("mov [bp + di], byte 7", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn immediate_to_register_instruction_word() {
        let encoded_instruction = [199, 133, 133, 3, 91, 1];
        let decoded_instruction = decode(&encoded_instruction).unwrap();
        assert_eq!("mov [di + 901], word 347", decoded_instruction.0);
        assert_eq!(6, decoded_instruction.1);
    }

    #[test]
    fn test_memory_to_accumulator() {
        let encoded_instruction = [161, 251, 9];
        let decoded_instruction = decode(&encoded_instruction).unwrap();
        assert_eq!("mov ax, [2555]", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);

        let encoded_instruction = [161, 16, 0];
        let decoded_instruction = decode(&encoded_instruction).unwrap();
        assert_eq!("mov ax, [16]", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);
    }

    #[test]
    fn test_accumulator_to_memory() {
        let encoded_instruction = [163, 15, 0];
        let decoded_instruction = decode(&encoded_instruction).unwrap();
        assert_eq!("mov [15], ax", decoded_instruction.0);
        assert_eq!(3, decoded_instruction.1);

        let encoded_instruction = [163, 250, 9];
        let decoded_instruction = decode(&encoded_instruction).unwrap();
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
        assert_eq!(
            Ok(OpCode::RmToOrFromRegister),
            decode_opcode(&0b10001001, None)
        );
        assert_eq!(
            Ok(OpCode::RmToOrFromRegister),
            decode_opcode(&0b10001011, None)
        );
        assert_eq!(
            Ok(OpCode::RmToOrFromRegister),
            decode_opcode(&0b10001010, None)
        );
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
    fn test_decode_first_register() {
        // Case byte
        let first_byte_with_byte_mode: u8 = 0b10001000;
        assert_eq!(
            "al",
            decode_reg_field(&first_byte_with_byte_mode, &0b00000000)
        );
        assert_eq!(
            "cl",
            decode_reg_field(&first_byte_with_byte_mode, &0b00001000)
        );
        assert_eq!(
            "dl",
            decode_reg_field(&first_byte_with_byte_mode, &0b00010000)
        );
        assert_eq!(
            "bl",
            decode_reg_field(&first_byte_with_byte_mode, &0b00011000)
        );
        assert_eq!(
            "ah",
            decode_reg_field(&first_byte_with_byte_mode, &0b00100000)
        );
        assert_eq!(
            "ch",
            decode_reg_field(&first_byte_with_byte_mode, &0b00101000)
        );
        assert_eq!(
            "dh",
            decode_reg_field(&first_byte_with_byte_mode, &0b00110000)
        );
        assert_eq!(
            "bh",
            decode_reg_field(&first_byte_with_byte_mode, &0b00111000)
        );

        let first_byte_with_word_mode: u8 = 0b10001001;
        assert_eq!(
            "ax",
            decode_reg_field(&first_byte_with_word_mode, &0b00000000)
        );
        assert_eq!(
            "cx",
            decode_reg_field(&first_byte_with_word_mode, &0b00001000)
        );
        assert_eq!(
            "dx",
            decode_reg_field(&first_byte_with_word_mode, &0b00010000)
        );
        assert_eq!(
            "bx",
            decode_reg_field(&first_byte_with_word_mode, &0b00011000)
        );
        assert_eq!(
            "sp",
            decode_reg_field(&first_byte_with_word_mode, &0b00100000)
        );
        assert_eq!(
            "bp",
            decode_reg_field(&first_byte_with_word_mode, &0b00101000)
        );
        assert_eq!(
            "si",
            decode_reg_field(&first_byte_with_word_mode, &0b00110000)
        );
        assert_eq!(
            "di",
            decode_reg_field(&first_byte_with_word_mode, &0b00111000)
        );
    }

    #[test]
    fn test_decode_second_register() {
        // Case byte
        assert_eq!(
            "al",
            decode_rm_field(WordByteOperation::Byte, &0b11000000, None, None).0
        );
        assert_eq!(
            "cl",
            decode_rm_field(WordByteOperation::Byte, &0b11000001, None, None).0
        );
        assert_eq!(
            "dl",
            decode_rm_field(WordByteOperation::Byte, &0b11000010, None, None).0
        );
        assert_eq!(
            "bl",
            decode_rm_field(WordByteOperation::Byte, &0b11000011, None, None).0
        );
        assert_eq!(
            "ah",
            decode_rm_field(WordByteOperation::Byte, &0b11000100, None, None).0
        );
        assert_eq!(
            "ch",
            decode_rm_field(WordByteOperation::Byte, &0b11000101, None, None).0
        );
        assert_eq!(
            "dh",
            decode_rm_field(WordByteOperation::Byte, &0b11000110, None, None).0
        );
        assert_eq!(
            "bh",
            decode_rm_field(WordByteOperation::Byte, &0b11000111, None, None).0
        );

        assert_eq!(
            "ax",
            decode_rm_field(WordByteOperation::Word, &0b11000000, None, None).0
        );
        assert_eq!(
            "cx",
            decode_rm_field(WordByteOperation::Word, &0b11000001, None, None).0
        );
        assert_eq!(
            "dx",
            decode_rm_field(WordByteOperation::Word, &0b11000010, None, None).0
        );
        assert_eq!(
            "bx",
            decode_rm_field(WordByteOperation::Word, &0b11000011, None, None).0
        );
        assert_eq!(
            "sp",
            decode_rm_field(WordByteOperation::Word, &0b11000100, None, None).0
        );
        assert_eq!(
            "bp",
            decode_rm_field(WordByteOperation::Word, &0b11000101, None, None).0
        );
        assert_eq!(
            "si",
            decode_rm_field(WordByteOperation::Word, &0b11000110, None, None).0
        );
        assert_eq!(
            "di",
            decode_rm_field(WordByteOperation::Word, &0b11000111, None, None).0
        );
    }
}
