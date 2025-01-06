#[allow(unused_imports)]
use std::{env, fs, io};

mod deassembler;
mod simulator;
fn main() {
    //  deassembler::deassembler::deassembly()
    run_simulate();
}

fn run_simulate() {
    let args: Vec<String> = env::args().collect();
    if args.get(1).is_none() {
        panic!("Please provide assembly file path as a paramter");
    }
    let file_path = args.get(1).unwrap();

    let contents = fs::read(file_path);

    println!("Binary: {:?}", contents);
    match contents {
        Ok(contents) => {
            let Ok(result) = simulator::simulate_from_binary(&contents) else {
                panic!("Cannot simulate");
            };
            const REGISTERS: [&str; 11] = ["ax", "bx", "cx", "dx", "sp", "bp", "si", "di", "es", "ss", "ds"];
            for register in REGISTERS {
                let value = result.get(register).unwrap_or(&0);
                println!("{:?}: {:#04x}", register, value);
            }
        }
        Err(err) => println!("Error loading file: {}", err),
    }
}
