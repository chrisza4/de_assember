#[allow(unused_imports)]
use std::{env, fs, io};

use simulator::flags::Flags;

mod deassembler;
mod simulator;
mod binary;

fn main() {
    run_simulate();
}

#[allow(clippy::println_empty_string)]
fn run_simulate() {
    let args: Vec<String> = env::args().collect();
    if args.get(1).is_none() {
        panic!("Please provide assembly file path as a paramter");
    }
    let file_path = args.get(1).unwrap();

    let contents = fs::read(file_path);

    match contents {
        Ok(contents) => {
            for bits in &contents {
                print!("{:#010b},", bits);
            };
            println!("");
            let Ok(result) = simulator::simulate_from_binary(&contents) else {
                panic!("Cannot simulate");
            };
            const REGISTERS: [&str; 11] = [
                "ax", "bx", "cx", "dx", "sp", "bp", "si", "di", "es", "ss", "ds",
            ];
            for register in REGISTERS {
                let value = result.register.get(register).unwrap_or(&0);
                println!("{:?}: {:#06x}", register, value);
            }
            println!("Flags: {}", result.flags.get_all_flags_sorted());
            println!("IP: {:#06x} ({})", result.pointer, result.pointer);
        }
        Err(err) => println!("Error loading file: {}", err),
    }
}
