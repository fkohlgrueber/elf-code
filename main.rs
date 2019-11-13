// elf code stuff
pub struct Program {

}

#[derive(Debug)]
pub enum Reg {
    Ip,
    Reg(u8)
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::Ip => write!(f, "ip"),
            Reg::Reg(r) => write!(f, "r{}", r)
        }
    }
}

impl Reg {
    pub fn from_str(s: &str, ip_idx: u8) -> Reg {
        let r = s.parse().unwrap();
        if r == ip_idx {
            Reg::Ip
        } else {
            Reg::Reg(r)
        }
    }
}

#[derive(Debug)]
pub struct Imm(usize);

impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:2}", self.0)
    }
}


impl Imm {
    pub fn from_str(s: &str) -> Self {
        Self(s.parse().unwrap())
    }
}

#[derive(Debug)]
pub struct Instruction {
    opcode: OpCode,
    target: Reg,
}

impl Instruction {
    fn from_str(s: &str, ip_reg: u8) -> Instruction {
        let parts = s.split(" ").collect::<Vec<_>>();
        let a = parts[1];
        let b = parts[2];
        let c = Reg::from_str(parts[3], ip_reg);
        let opcode = match parts[0] {
            "addr"  => OpCode::Addr(Reg::from_str(a, ip_reg), Reg::from_str(b, ip_reg)),
            "addi"  => OpCode::Addi(Reg::from_str(a, ip_reg), Imm::from_str(b)),
            "mulr"  => OpCode::Mulr(Reg::from_str(a, ip_reg), Reg::from_str(b, ip_reg)),
            "muli"  => OpCode::Muli(Reg::from_str(a, ip_reg), Imm::from_str(b)),
            "setr"  => OpCode::Setr(Reg::from_str(a, ip_reg)),
            "seti"  => OpCode::Seti(Imm::from_str(a)),
            "gtrr"  => OpCode::Gtrr(Reg::from_str(a, ip_reg), Reg::from_str(b, ip_reg)),
            "eqrr"  => OpCode::Eqrr(Reg::from_str(a, ip_reg), Reg::from_str(b, ip_reg)),
            _ => unimplemented!()
        };
        Instruction {
            opcode,
            target: c
        }
    }

    fn print(&self) -> String {
        let op_str = match &self.opcode {
            OpCode::Addr(a, b) => format!("addr {} {}", a, b),
            OpCode::Addi(a, b) => format!("addi {} {}", a, b),
            OpCode::Mulr(a, b) => format!("mulr {} {}", a, b),
            OpCode::Muli(a, b) => format!("muli {} {}", a, b),
            OpCode::Setr(a) => format!("setr {}   ", a),
            OpCode::Seti(a) => format!("seti {}   ", a),
            OpCode::Gtrr(a, b) => format!("gtrr {} {}", a, b),
            OpCode::Eqrr(a, b) => format!("eqrr {} {}", a, b),
        };
        format!("{} {}", op_str, self.target)
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
    let ip_reg: u8 = input.lines().next().unwrap().trim_start_matches("#ip ").parse().unwrap();
    let insts: Vec<_> = input.lines().skip(1).map(|l| Instruction::from_str(l, ip_reg)).collect();
    for (idx, i) in insts.iter().enumerate() {
        println!("{:02}  {}", idx, i.print());
    }
}