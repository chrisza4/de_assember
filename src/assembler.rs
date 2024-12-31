use crate::decoder::decode;

pub fn decode_asm_binary(binary: &Vec<u8>) -> String {
    let processing_binary = binary.clone();
    let mut iterator = processing_binary.iter().peekable();
    let mut result = String::new();
    result.push_str("bits 16\n\n");
    while (iterator.peek().is_some()) {
        let current_chunk: Vec<&u8> = iterator.clone().take(4).collect::<Vec<_>>();
        let (asm_instruction, bytes_consumed) = decode(&current_chunk).unwrap();
        result.push_str(&asm_instruction);
        result.push('\n');
        (0..bytes_consumed).for_each(|_| {
            iterator.next();
        });
    }
    result
}
