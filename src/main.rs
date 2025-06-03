mod alu;
mod decoder;
mod logic;
mod rom;

use decoder::decode_dispatch;
use logic::Procedual;
use rom::ROM;

struct Core {
    rom: ROM<1024>,
}

impl Core {
    fn new() -> Self {
        Self {
            rom: ROM::new("program/build/test.mem".to_string()).unwrap(),
        }
    }
}

impl Procedual for Core {
    fn posedge_clk(&mut self) {
        let inst = self.rom.read(0);
        let decoded = decode_dispatch(inst);

        println!("{:?}", decoded);
    }
}

fn main() {
    let mut core = Core::new();
    core.posedge_clk();
    // todo: replace with loop here to simulate clock
}
