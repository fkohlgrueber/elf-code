use std::fmt::Display;
use std::ops::Add;

/// Struct representing an immediate value.
#[derive(Debug, Clone, Copy)]
pub struct Imm(usize);

impl Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:3}", self.0)
    }
}

impl Add<usize> for Imm {
    type Output = usize;

    fn add(self, other: usize) -> usize {
        self.0 + other
    }
}

impl Add<usize> for &Imm {
    type Output = usize;

    fn add(self, other: usize) -> usize {
        self.0 + other
    }
}

impl Imm {
    pub fn from_str(s: &str) -> Self {
        Self(s.parse().unwrap())
    }
}

/// Struct representing a register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Reg(pub u8);

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, " r{}", self.0)
    }
}


/// Enum that is either a register or the instruction pointer.
#[derive(Debug, Clone, Copy)]
pub enum RegOrIp {
    Ip,
    Reg(Reg)
}

impl Display for RegOrIp {
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

/// An instruction that consists of an opcode (containing the operation and 
/// operands) and a target register.
#[derive(Debug, Clone)]
pub struct Instruction<R, TR> 
where R: Display, TR: Display
{
    pub opcode: OpCode<R>,
    pub target: TR,
}

impl<R, TR> Display for Instruction<R, TR> 
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
            "gtir"  => OpCode::Gtir(Imm::from_str(a), RegOrIp::from_str(b, ip_reg)),
            "gtri"  => OpCode::Gtri(RegOrIp::from_str(a, ip_reg), Imm::from_str(a)),
            "gtrr"  => OpCode::Gtrr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "eqir"  => OpCode::Eqir(Imm::from_str(a), RegOrIp::from_str(b, ip_reg)),
            "eqri"  => OpCode::Eqri(RegOrIp::from_str(a, ip_reg), Imm::from_str(a)),
            "eqrr"  => OpCode::Eqrr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "banr"  => OpCode::Banr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "bani"  => OpCode::Bani(RegOrIp::from_str(a, ip_reg), Imm::from_str(b)),
            "borr"  => OpCode::Borr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "bori"  => OpCode::Bori(RegOrIp::from_str(a, ip_reg), Imm::from_str(b)),
            _ => panic!("Invalid input")
        };
        Instruction {
            opcode,
            target: c
        }
    }
}

impl<TR> Instruction<RegOrIp, TR>
where TR: Display {
    pub fn inline_ip_lhs(self, ip_value: usize) -> Instruction<Reg, TR> {
        Instruction {
            opcode: self.opcode.inline_ip_lhs(ip_value),
            target: self.target
        }
    }
}

impl Instruction<Reg, Reg> {
    pub fn pretty_print(&self) -> String {
        let rhs = match self.opcode {
            OpCode::Addr(a, b) => format!("r{} + r{}", a.0, b.0),
            OpCode::Addi(a, b) => format!("r{} + {} (0x{:x})", a.0, b.0, b.0),
            OpCode::Mulr(a, b) => format!("r{} * r{}", a.0, b.0),
            OpCode::Muli(a, b) => format!("r{} * {} (0x{:x})", a.0, b.0, b.0),
            OpCode::Setr(a) => format!("r{}", a.0),
            OpCode::Seti(a) => format!("{} (0x{:x})", a.0, a.0),
            OpCode::Gtir(a, b) => format!("{} (0x{:x}) > r{}", a.0, a.0, b.0),
            OpCode::Gtri(a, b) => format!("r{} > {} (0x{:x})", a.0, b.0, b.0),
            OpCode::Gtrr(a, b) => format!("r{} > r{}", a.0, b.0),
            OpCode::Eqir(a, b) => format!("{} (0x{:x}) == r{}", a.0, a.0, b.0),
            OpCode::Eqri(a, b) => format!("r{} == {} (0x{:x})", a.0, b.0, b.0),
            OpCode::Eqrr(a, b) => format!("r{} == r{}", a.0, b.0),
            OpCode::Banr(a, b) => format!("r{} & r{}", a.0, b.0),
            OpCode::Bani(a, b) => format!("r{} & {} (0x{:x})", a.0, b.0, b.0),
            OpCode::Borr(a, b) => format!("r{} | r{}", a.0, b.0),
            OpCode::Bori(a, b) => format!("r{} | {} (0x{:x})", a.0, b.0, b.0),
        };
        format!("{} = {}", self.target, rhs)
    }

    pub fn get_used_regs(&self) -> Vec<Reg> {
        match self.opcode {
            OpCode::Addr(a, b) => vec!(a, b),
            OpCode::Addi(a, _b) => vec!(a),
            OpCode::Mulr(a, b) => vec!(a, b),
            OpCode::Muli(a, _b) => vec!(a),
            OpCode::Setr(a) => vec!(a),
            OpCode::Seti(_a) => vec!(),
            OpCode::Gtir(_a, b) => vec!(b),
            OpCode::Gtri(a, _b) => vec!(a),
            OpCode::Gtrr(a, b) => vec!(a, b),
            OpCode::Eqir(_a, b) => vec!(b),
            OpCode::Eqri(a, _b) => vec!(a),
            OpCode::Eqrr(a, b) => vec!(a, b),
            OpCode::Banr(a, b) => vec!(a, b),
            OpCode::Bani(a, _b) => vec!(a),
            OpCode::Borr(a, b) => vec!(a, b),
            OpCode::Bori(a, _b) => vec!(a),
        }
    }
}

/// An enum that contains all valid elf-code instructions and their respective operands.
#[derive(Debug, Clone)]
pub enum OpCode<R> 
where R: Display
{
    Addr(R, R),
    Addi(R, Imm),
    Mulr(R, R),
    Muli(R, Imm),
    Banr(R, R),
    Bani(R, Imm),
    Borr(R, R),
    Bori(R, Imm),
    Setr(R),
    Seti(Imm),
    Gtir(Imm, R),
    Gtri(R, Imm),
    Gtrr(R, R),
    Eqir(Imm, R),
    Eqri(R, Imm),
    Eqrr(R, R),
}

impl<R> Display for OpCode<R> 
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
            OpCode::Gtir(a, b) => write!(f, "gtir {} {}", a, b),
            OpCode::Gtri(a, b) => write!(f, "gtri {} {}", a, b),
            OpCode::Gtrr(a, b) => write!(f, "gtrr {} {}", a, b),
            OpCode::Eqir(a, b) => write!(f, "eqir {} {}", a, b),
            OpCode::Eqri(a, b) => write!(f, "eqri {} {}", a, b),
            OpCode::Eqrr(a, b) => write!(f, "eqrr {} {}", a, b),
            OpCode::Banr(a, b) => write!(f, "banr {} {}", a, b),
            OpCode::Bani(a, b) => write!(f, "bani {} {}", a, b),
            OpCode::Borr(a, b) => write!(f, "borr {} {}", a, b),
            OpCode::Bori(a, b) => write!(f, "bori {} {}", a, b),
        }
    }
}

impl OpCode<RegOrIp> {
    pub fn inline_ip_lhs(self, ip_value: usize) -> OpCode<Reg> {
        match self {
            OpCode::Addi(RegOrIp::Ip, i) => OpCode::Seti(Imm(i.0+ip_value)),
            OpCode::Addi(RegOrIp::Reg(r), i) => OpCode::Addi(r, i),
            OpCode::Addr(RegOrIp::Ip, RegOrIp::Ip) => OpCode::Seti(Imm(ip_value+ip_value)),
            OpCode::Addr(RegOrIp::Ip, RegOrIp::Reg(r)) => OpCode::Addi(r, Imm(ip_value)),
            OpCode::Addr(RegOrIp::Reg(r), RegOrIp::Ip) => OpCode::Addi(r, Imm(ip_value)),
            OpCode::Addr(RegOrIp::Reg(ra), RegOrIp::Reg(rb)) => OpCode::Addr(ra, rb),
            OpCode::Muli(RegOrIp::Ip, i) => OpCode::Seti(Imm(i.0*ip_value)),
            OpCode::Muli(RegOrIp::Reg(r), i) => OpCode::Muli(r, i),
            OpCode::Mulr(RegOrIp::Ip, RegOrIp::Ip) => OpCode::Seti(Imm(ip_value*ip_value)),
            OpCode::Mulr(RegOrIp::Ip, RegOrIp::Reg(r)) => OpCode::Muli(r, Imm(ip_value)),
            OpCode::Mulr(RegOrIp::Reg(r), RegOrIp::Ip) => OpCode::Muli(r, Imm(ip_value)),
            OpCode::Mulr(RegOrIp::Reg(ra), RegOrIp::Reg(rb)) => OpCode::Mulr(ra, rb),
            OpCode::Setr(RegOrIp::Ip) => OpCode::Seti(Imm(ip_value)),
            OpCode::Setr(RegOrIp::Reg(r)) => OpCode::Setr(r),
            OpCode::Seti(i) => OpCode::Seti(i),
            OpCode::Gtrr(RegOrIp::Ip, RegOrIp::Ip) => OpCode::Seti(Imm(0)),
            OpCode::Gtrr(RegOrIp::Reg(r), RegOrIp::Ip) => OpCode::Gtri(r, Imm(ip_value)),
            OpCode::Gtrr(RegOrIp::Ip, RegOrIp::Reg(r)) => OpCode::Gtir(Imm(ip_value), r),
            OpCode::Gtrr(RegOrIp::Reg(ra), RegOrIp::Reg(rb)) => OpCode::Gtrr(ra, rb),
            OpCode::Eqrr(RegOrIp::Ip, RegOrIp::Ip) => OpCode::Seti(Imm(1)),
            OpCode::Eqrr(RegOrIp::Reg(r), RegOrIp::Ip) => OpCode::Eqri(r, Imm(ip_value)),
            OpCode::Eqrr(RegOrIp::Ip, RegOrIp::Reg(r)) => OpCode::Eqir(Imm(ip_value), r),
            OpCode::Eqrr(RegOrIp::Reg(ra), RegOrIp::Reg(rb)) => OpCode::Eqrr(ra, rb),
            OpCode::Gtir(i, RegOrIp::Ip) => OpCode::Seti(Imm((i.0 > ip_value) as usize)),
            OpCode::Gtir(i, RegOrIp::Reg(r)) => OpCode::Gtir(i, r),
            OpCode::Gtri(RegOrIp::Ip, i) => OpCode::Seti(Imm((ip_value > i.0) as usize)),
            OpCode::Gtri(RegOrIp::Reg(r), i) => OpCode::Gtri(r, i),
            OpCode::Eqir(i, RegOrIp::Ip) => OpCode::Seti(Imm((i.0 == ip_value) as usize)),
            OpCode::Eqir(i, RegOrIp::Reg(r)) => OpCode::Eqir(i, r),
            OpCode::Eqri(RegOrIp::Ip, i) => OpCode::Seti(Imm((ip_value == i.0) as usize)),
            OpCode::Eqri(RegOrIp::Reg(r), i) => OpCode::Eqri(r, i),
            OpCode::Bani(RegOrIp::Ip, i) => OpCode::Seti(Imm(i.0 & ip_value)),
            OpCode::Bani(RegOrIp::Reg(r), i) => OpCode::Bani(r, i),
            OpCode::Banr(RegOrIp::Ip, RegOrIp::Ip) => OpCode::Seti(Imm(ip_value & ip_value)),
            OpCode::Banr(RegOrIp::Ip, RegOrIp::Reg(r)) => OpCode::Bani(r, Imm(ip_value)),
            OpCode::Banr(RegOrIp::Reg(r), RegOrIp::Ip) => OpCode::Bani(r, Imm(ip_value)),
            OpCode::Banr(RegOrIp::Reg(ra), RegOrIp::Reg(rb)) => OpCode::Banr(ra, rb),
            OpCode::Bori(RegOrIp::Ip, i) => OpCode::Seti(Imm(i.0 | ip_value)),
            OpCode::Bori(RegOrIp::Reg(r), i) => OpCode::Bori(r, i),
            OpCode::Borr(RegOrIp::Ip, RegOrIp::Ip) => OpCode::Seti(Imm(ip_value | ip_value)),
            OpCode::Borr(RegOrIp::Ip, RegOrIp::Reg(r)) => OpCode::Bori(r, Imm(ip_value)),
            OpCode::Borr(RegOrIp::Reg(r), RegOrIp::Ip) => OpCode::Bori(r, Imm(ip_value)),
            OpCode::Borr(RegOrIp::Reg(ra), RegOrIp::Reg(rb)) => OpCode::Borr(ra, rb),
        }
    }
}

#[derive(Debug)]
pub struct Program<R, TR>
where R: Display, TR: Display 
{
    pub insts: Vec<Instruction<R, TR>>
}

impl<R, TR> Display for Program<R, TR> 
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

impl<TR> Program<RegOrIp, TR> 
where TR: Display {
    pub fn inline_ip_lhs(self) -> Program<Reg, TR> {
        let insts =  self.insts.into_iter().enumerate().map(|(idx, inst)| inst.inline_ip_lhs(idx)).collect();
        Program {
            insts
        }
    }
}

