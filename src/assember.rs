use crate::decoder::decode;

pub fn decode_asm_binary(binary: &Vec<u8>) -> String {
  let instructions: Vec<_> = binary.chunks(2).collect();
  let mut result = String::new();
  result.push_str("bits 16\n\n");
  for instruction in &instructions {
    let asm_instruction = decode(&instruction).unwrap();
    result.push_str(&asm_instruction);
    result.push('\n');
  }
  result
}
