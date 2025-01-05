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

    let contents = fs::read_to_string(file_path);

    match contents {
        Ok(contents) => {
            let Ok(result) = simulator::simulate(contents) else {
                panic!("Cannot simulate");
            };
            const REGISTERS: [&str; 8] = ["ax", "bx", "cx", "dx", "sp", "bp", "si", "di"];
            for register in REGISTERS {
                let value = result.get(register).unwrap_or(&0);
                println!("{:?}: {:#04x}", register, value);
            }
        }
        Err(err) => println!("Error loading file: {}", err),
    }
}
