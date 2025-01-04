use std::io;

mod deassembler;
mod decoder;
mod binary;
fn main() -> io::Result<()> {
    deassembler::deassembly()
}
