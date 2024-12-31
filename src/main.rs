use std::env;
use std::fs;
use std::io;
use std::process::Command;
use assember::decode_asm_binary;

mod assember;
mod decoder;
mod binary;
fn main() -> io::Result<()> {
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
        Err(err) => println!("Error: {}", err)
    }
}
