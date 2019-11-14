

use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct Imm(usize);

impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:3}", self.0)
    }
}

impl Imm {
    pub fn from_str(s: &str) -> Self {
        Self(s.parse().unwrap())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Reg(u8);

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, " r{}", self.0)
    }
}


#[derive(Debug, Clone, Copy)]
pub enum RegOrIp {
    Ip,
    Reg(Reg)
}

impl std::fmt::Display for RegOrIp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegOrIp::Ip => write!(f, " ip"),
            RegOrIp::Reg(r) => write!(f, "{:3}", r)
        }
    }
}

impl RegOrIp {
    pub fn from_str(s: &str, ip_idx: u8) -> RegOrIp {
        let r = s.parse().unwrap();
        if r == ip_idx {
            RegOrIp::Ip
        } else {
            RegOrIp::Reg(Reg(r))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction<R, TR> 
where R: Display, TR: Display
{
    opcode: OpCode<R>,
    target: TR,
}

impl<R, TR> std::fmt::Display for Instruction<R, TR> 
where R: Display, TR: Display 
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.opcode, self.target)
    }
}

impl Instruction<RegOrIp, RegOrIp> {
    pub fn from_str(s: &str, ip_reg: u8) -> Instruction<RegOrIp, RegOrIp> {
        let parts = s.split(" ").collect::<Vec<_>>();
        let a = parts[1];
        let b = parts[2];
        let c = RegOrIp::from_str(parts[3], ip_reg);
        let opcode = match parts[0] {
            "addr"  => OpCode::Addr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "addi"  => OpCode::Addi(RegOrIp::from_str(a, ip_reg), Imm::from_str(b)),
            "mulr"  => OpCode::Mulr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "muli"  => OpCode::Muli(RegOrIp::from_str(a, ip_reg), Imm::from_str(b)),
            "setr"  => OpCode::Setr(RegOrIp::from_str(a, ip_reg)),
            "seti"  => OpCode::Seti(Imm::from_str(a)),
            "gtrr"  => OpCode::Gtrr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "eqrr"  => OpCode::Eqrr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            _ => unimplemented!()
        };
        Instruction {
            opcode,
            target: c
        }
    }
}

#[derive(Debug, Clone)]
pub enum OpCode<R> 
where R: Display
{
    Addr(R, R),
    Addi(R, Imm),
    Mulr(R, R),
    Muli(R, Imm),
    //Banr(R, R),
    //Bani(R, Imm),
    //Borr(R, R),
    //Bori(R, Imm),
    Setr(R),
    Seti(Imm),
    //Gtir(Imm, R),
    //Gtri(R, Imm),
    Gtrr(R, R),
    //Eqir(Imm, R),
    //Eqri(R, Imm),
    Eqrr(R, R),
}

impl<R> std::fmt::Display for OpCode<R> 
where R: Display
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Addr(a, b) => write!(f, "addr {} {}", a, b),
            OpCode::Addi(a, b) => write!(f, "addi {} {}", a, b),
            OpCode::Mulr(a, b) => write!(f, "mulr {} {}", a, b),
            OpCode::Muli(a, b) => write!(f, "muli {} {}", a, b),
            OpCode::Setr(a) => write!(f, "setr {}    ", a),
            OpCode::Seti(a) => write!(f, "seti {}    ", a),
            OpCode::Gtrr(a, b) => write!(f, "gtrr {} {}", a, b),
            OpCode::Eqrr(a, b) => write!(f, "eqrr {} {}", a, b),
        }
    }
}

#[derive(Debug)]
pub struct Program<R, TR>
where R: Display, TR: Display 
{
    insts: Vec<Instruction<R, TR>>
}

impl<R, TR> std::fmt::Display for Program<R, TR> 
where R: Display, TR: Display 
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Program {{")?;
        for (idx, inst) in self.insts.iter().enumerate() {
            writeln!(f, "  {:02}  {}", idx, inst)?;
        }
        writeln!(f, "}}")
    }
}

impl Program<RegOrIp, RegOrIp> {
    pub fn from_str(s: &str) -> Self {
        let mut lines = s.lines();
        let ip_reg: u8 = lines.next().unwrap().trim_start_matches("#ip ").parse().unwrap();
        let insts = lines.map(|l| Instruction::from_str(l, ip_reg)).collect();
        Program{
            insts
        }
    }
}





fn main() {
    let input = std::fs::read_to_string("00_orig.txt").unwrap();
    let program = Program::from_str(&input);
    println!("{}", program);
}