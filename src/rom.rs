use std::fs::File;
use std::io::{self, BufRead};

pub struct ROM<const N: usize> {
    instrs: [u32; N],
}

use std::path::PathBuf;

impl<const N: usize> ROM<N> {
    pub fn new(relative_file_path: String) -> Result<Self, io::Error> {
        let mut instrs = [0u32; N];

        // Construct path relative to CARGO_MANIFEST_DIR
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push(relative_file_path);

        // Read instructions from file
        let lines = {
            let file = File::open(path)?;
            io::BufReader::new(file).lines()
        };

        lines
            .take(N) // Only take up to N lines to avoid exceeding array bounds
            .enumerate()
            .filter_map(|(i, line_result)| {
                line_result.ok().and_then(|line| {
                    u32::from_str_radix(line.trim(), 16).ok().map(|instr| (i, instr))
                })
            })
            .for_each(|(i, instr)| {
                instrs[i] = instr;
            });
        
        Ok(Self { instrs })
    }
    
    pub fn read(&self, addr: usize) -> u32 {
        if addr < N {
            self.instrs[addr]
        } else {
            0 // Return 0 for out-of-bounds addresses
        }
    }
}