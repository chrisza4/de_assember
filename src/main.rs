use std::io;

mod deassembler;
fn main() -> io::Result<()> {
   deassembler::deassembler::deassembly()
}
