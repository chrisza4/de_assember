use std::{env, fs, io, process::Command};

use decoder::decode;

use super::decoder;

pub fn deassembly() -> io::Result<()> {
  let args: Vec<String> = env::args().collect();
  if args.get(1).is_none() {
      panic!("Please provide assembly file path as a paramter");
  }
  let file_path = args.get(1).unwrap();

  let contents = fs::read(file_path);

  match contents {
      Ok(contents) => {
          let de_asm_content = decode_asm_binary(&contents);
          println!("Decoded assembly:\n{}", de_asm_content);
          write_and_validate_result(&contents, &de_asm_content);
      }
      Err(err) => println!("Error loading file: {}", err),
  }

  Ok(())
}

fn write_and_validate_result(original_contents: &Vec<u8>, de_asm_content: &String) {
  let write_result = fs::write("out.asm", de_asm_content.as_bytes());
  match write_result {
      Ok(_) => {
          println!("Write to out.asm success!");
          Command::new("nasm").arg("./out.asm").output().expect("");
          let new_contents = fs::read("./out").unwrap();
          if *original_contents == new_contents {
              println!("\nValidated new assembly. Same. We got this!");
          } else {
              println!("\nError: Difference between new and old assembly");
          }
      }
      Err(err) => println!("Error: {}", err),
  }
}

fn decode_asm_binary(binary: &Vec<u8>) -> String {
  println!("{:?}", binary);
  let processing_binary = binary.clone();
  let mut iterator = processing_binary.iter().peekable();
  let mut result = String::new();
  result.push_str("bits 16\n\n");
  while iterator.peek().is_some() {
      let current_chunk: Vec<&u8> = iterator.clone().take(6).collect::<Vec<_>>();
      let (asm_instruction, bytes_consumed) = decode(&current_chunk).unwrap();
      println!("{}", asm_instruction);
      result.push_str(&asm_instruction);
      result.push('\n');
      (0..bytes_consumed).for_each(|_| {
          iterator.next();
      });
  }
  result
}
