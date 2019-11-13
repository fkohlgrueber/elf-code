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
            Reg::Ip => write!(f, " ip"),
            Reg::Reg(r) => write!(f, " r{}", r)
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
        write!(f, "{:3}", self.0)
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
            OpCode::Setr(a) => format!("setr {}    ", a),
            OpCode::Seti(a) => format!("seti {}    ", a),
            OpCode::Gtrr(a, b) => format!("gtrr {} {}", a, b),
            OpCode::Eqrr(a, b) => format!("eqrr {} {}", a, b),
        };
        format!("{} {}", op_str, self.target)
    }

    fn is_jump(&self) -> bool {
        match self.target {
            Reg::Ip => true,
            _ => false
        }
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
    let insts = inline_ip(insts);
    for (idx, i) in insts.iter().enumerate() {
        println!("{:02}  {}", idx, i.print());
    }
    analyze_jmp_targets(insts);
}

fn inline_ip(insts: Vec<Instruction>) -> Vec<Instruction>{
    insts.into_iter().enumerate().map(
        |(idx, mut inst)| {
            inst.opcode = match inst.opcode {
                OpCode::Addi(Reg::Ip, i) => OpCode::Seti(Imm(i.0+idx)),
                OpCode::Addr(Reg::Ip, Reg::Ip) => OpCode::Seti(Imm(idx+idx)),
                OpCode::Addr(Reg::Ip, r) => OpCode::Addi(r, Imm(idx)),
                OpCode::Addr(r, Reg::Ip) => OpCode::Addi(r, Imm(idx)),
                OpCode::Mulr(Reg::Ip, Reg::Ip) => OpCode::Seti(Imm(idx*idx)),
                OpCode::Muli(Reg::Ip, i) => OpCode::Seti(Imm(i.0*idx)),
                OpCode::Mulr(Reg::Ip, r) => OpCode::Muli(r, Imm(idx)),
                OpCode::Mulr(r, Reg::Ip) => OpCode::Muli(r, Imm(idx)),
                OpCode::Setr(Reg::Ip) => OpCode::Seti(Imm(idx)),
                op => op
            };
            inst
        }
    ).collect()
}

fn analyze_jmp_targets(insts: Vec<Instruction>) {
    let mut queue = vec!();
    let mut targets: std::collections::HashSet<usize> = std::collections::HashSet::new();
    queue.push(0);
    'outer: while let Some(mut idx) = queue.pop() {
        if idx >= insts.len() {
            idx = usize::max_value();
        }
        if targets.contains(&idx) {
            continue;
        }
        targets.insert(idx);
        let mut regs = [RegStatus::Any; 6];
        while idx < insts.len() {
            let inst = &insts[idx];
            match inst.target {
                Reg::Ip => {
                    // jump
                    match &inst.opcode {
                        OpCode::Seti(i) => queue.push(i.0+1),
                        OpCode::Addi(r, i) => {
                            let reg = if let Reg::Reg(n) = r { n } else { panic!()};
                            match regs[*reg as usize] {
                                RegStatus::Any => {
                                    for index in i.0+1..=insts.len() {
                                        queue.push(index);
                                    }
                                }
                                RegStatus::Bin => {
                                    queue.push(i.0+1);
                                    queue.push(i.0+2);
                                }
                            }
                        },
                        _ => unimplemented!()
                    }
                    break;
                },
                Reg::Reg(n) => {
                    // no jump
                    regs[n as usize] = match inst.opcode {
                        OpCode::Gtrr(_, _) | OpCode::Eqrr(_, _) => RegStatus::Bin,
                        _ => RegStatus::Any
                    };
                }
            }
            idx += 1;
        }
    }
    let mut sorted = targets.iter().collect::<Vec<_>>();
    sorted.sort();
    println!("{:?}", sorted);
}

#[derive(Clone, Copy)]
pub enum RegStatus {
    Any,
    Bin
}