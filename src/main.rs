use std::io;

mod deassembler;
mod simulator;
fn main() -> io::Result<()> {
   deassembler::deassembler::deassembly()
}
