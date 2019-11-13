// elf code stuff
use std::convert::TryInto;

pub struct Program {

}

type Reg = u8;
type Imm = usize;

#[derive(Debug)]
pub struct Instruction {
    opcode: OpCode,
    target: Reg,
}

impl Instruction {
    fn from_str(s: &str) -> Instruction {
        let parts = s.split(" ").collect::<Vec<_>>();
        let a: usize = parts[1].parse().unwrap();
        let b: usize = parts[2].parse().unwrap();
        let c = parts[3].parse().unwrap();
        let opcode = match parts[0] {
            "addr"  => OpCode::Addr(a.try_into().unwrap(), b.try_into().unwrap()),
            "addi"  => OpCode::Addi(a.try_into().unwrap(), b.try_into().unwrap()),
            "mulr"  => OpCode::Mulr(a.try_into().unwrap(), b.try_into().unwrap()),
            "muli"  => OpCode::Muli(a.try_into().unwrap(), b.try_into().unwrap()),
            "setr"  => OpCode::Setr(a.try_into().unwrap()),
            "seti"  => OpCode::Seti(a.try_into().unwrap()),
            "gtrr"  => OpCode::Gtrr(a.try_into().unwrap(), b.try_into().unwrap()),
            "eqrr"  => OpCode::Eqrr(a.try_into().unwrap(), b.try_into().unwrap()),
            
            _ => unimplemented!()
        };
        Instruction {
            opcode,
            target: c
        }
    }

    fn print(&self) -> String {
        let op_str = match &self.opcode {
            OpCode::Addr(a, b) => format!("addr r{} r{}", a, b),
            OpCode::Addi(a, b) => format!("addi r{} {:2}", a, b),
            OpCode::Mulr(a, b) => format!("mulr r{} r{}", a, b),
            OpCode::Muli(a, b) => format!("muli r{} {:2}", a, b),
            OpCode::Setr(a) => format!("setr r{}   ", a),
            OpCode::Seti(a) => format!("seti {:2}   ", a),
            OpCode::Gtrr(a, b) => format!("gtrr r{} r{}", a, b),
            OpCode::Eqrr(a, b) => format!("eqrr r{} r{}", a, b),
        };
        format!("{} r{}", op_str, self.target)
    }
}

#[derive(Debug)]
pub enum OpCode {
    Addr(Reg, Reg),
    Addi(Reg, Imm),
    Mulr(Reg, Reg),
    Muli(Reg, Imm),
    //Banr(Reg, Reg),
    //Bani(Reg, Imm),
    //Borr(Reg, Reg),
    //Bori(Reg, Imm),
    Setr(Reg),
    Seti(Imm),
    //Gtir(Imm, Reg),
    //Gtri(Reg, Imm),
    Gtrr(Reg, Reg),
    //Eqir(Imm, Reg),
    //Eqri(Reg, Imm),
    Eqrr(Reg, Reg),
}

fn main() {
    let input = std::fs::read_to_string("00_orig.txt").unwrap();
    let ip_reg: usize = input.lines().next().unwrap().trim_start_matches("#ip ").parse().unwrap();
    let insts: Vec<_> = input.lines().skip(1).map(|l| Instruction::from_str(l)).collect();
    for (idx, i) in insts.iter().enumerate() {
        println!("{:02}  {}", idx, i.print());
    }
}