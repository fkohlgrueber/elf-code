

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

#[derive(Debug, Clone, Copy, PartialEq)]
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
            "gtir"  => OpCode::Gtir(Imm::from_str(a), RegOrIp::from_str(b, ip_reg)),
            "gtri"  => OpCode::Gtri(RegOrIp::from_str(a, ip_reg), Imm::from_str(a)),
            "gtrr"  => OpCode::Gtrr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            "eqir"  => OpCode::Eqir(Imm::from_str(a), RegOrIp::from_str(b, ip_reg)),
            "eqri"  => OpCode::Eqri(RegOrIp::from_str(a, ip_reg), Imm::from_str(a)),
            "eqrr"  => OpCode::Eqrr(RegOrIp::from_str(a, ip_reg), RegOrIp::from_str(b, ip_reg)),
            _ => unimplemented!()
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
    Gtir(Imm, R),
    Gtri(R, Imm),
    Gtrr(R, R),
    Eqir(Imm, R),
    Eqri(R, Imm),
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
            OpCode::Gtir(a, b) => write!(f, "gtir {} {}", a, b),
            OpCode::Gtri(a, b) => write!(f, "gtri {} {}", a, b),
            OpCode::Gtrr(a, b) => write!(f, "gtrr {} {}", a, b),
            OpCode::Eqir(a, b) => write!(f, "eqir {} {}", a, b),
            OpCode::Eqri(a, b) => write!(f, "eqri {} {}", a, b),
            OpCode::Eqrr(a, b) => write!(f, "eqrr {} {}", a, b),
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

impl<TR> Program<RegOrIp, TR> 
where TR: Display {
    pub fn inline_ip_lhs(self) -> Program<Reg, TR> {
        let insts =  self.insts.into_iter().enumerate().map(|(idx, inst)| inst.inline_ip_lhs(idx)).collect();
        Program {
            insts
        }
    }
}



#[derive(Debug, PartialEq)]
pub enum Jmp {
    Static(usize),
    Cond(Reg, usize, usize),  // register, jmp if true, jmp if false
    Vec(Reg, Vec<usize>),
}

impl Jmp {
    pub fn get_targets_mut(&mut self) -> Vec<&mut usize> {
        match self {
            Jmp::Static(n) => vec!(n),
            Jmp::Cond(_, a, b) => vec!(a, b),
            Jmp::Vec(_, v) => v.iter_mut().collect()
        }
    }

    pub fn get_targets(&self) -> Vec<&usize> {
        match self {
            Jmp::Static(n) => vec!(n),
            Jmp::Cond(_, a, b) => vec!(a, b),
            Jmp::Vec(_, v) => v.iter().collect()
        }
    }
}

#[derive(Clone, Copy)]
pub enum RegStatus {
    Any,
    Bin
}

pub struct Patch {
    pub insts: Vec<Instruction<Reg, Reg>>,
    pub jmp: Jmp,
}

pub struct PatchProgram {
    start: usize,
    patches: std::collections::HashMap<usize, Patch>
}

impl PatchProgram {
    pub fn from_program(program: Program<Reg, RegOrIp>) -> Self {
        let insts = program.insts;
        let mut queue = vec!();
        let mut targets: std::collections::HashMap<usize, Patch> = std::collections::HashMap::new();
        queue.push(0);
        'outer: while let Some(mut idx) = queue.pop() {
            if idx >= insts.len() {
                idx = usize::max_value();
            }
            if targets.contains_key(&idx) {
                continue;
            }
            let mut regs = [RegStatus::Any; 6];
            let idx_orig = idx;
            let mut tmp_instructions = vec!();
            while idx < insts.len() {
                let inst = &insts[idx];
                match inst.target {
                    RegOrIp::Ip => {
                        // jump
                        match &inst.opcode {
                            OpCode::Seti(i) => {
                                queue.push(i.0+1);
                                targets.insert(idx_orig, Patch{
                                    insts: tmp_instructions,
                                    jmp: Jmp::Static(i.0+1)
                                });
                            },
                            OpCode::Addi(Reg(r), i) => {
                                match regs[*r as usize] {
                                    RegStatus::Any => {
                                        for index in i.0+1..=insts.len() {
                                            queue.push(index);
                                        }
                                        targets.insert(idx_orig, Patch{
                                            insts: tmp_instructions,
                                            jmp: Jmp::Vec(Reg(*r), (i.0+1..=insts.len()).collect())
                                        });
                                    }
                                    RegStatus::Bin => {
                                        queue.push(i.0+1);
                                        queue.push(i.0+2);
                                        targets.insert(idx_orig, Patch{
                                            insts: tmp_instructions,
                                            jmp: Jmp::Cond(Reg(*r), i.0+2, i.0+1)
                                        });
                                    }
                                }
                            },
                            _ => unimplemented!()
                        }
                        break;
                    },
                    RegOrIp::Reg(Reg(n)) => {
                        // no jump
                        regs[n as usize] = match inst.opcode {
                            OpCode::Gtir(_, _) | 
                            OpCode::Gtri(_, _) | 
                            OpCode::Gtrr(_, _) | 
                            OpCode::Eqir(_, _) | 
                            OpCode::Eqri(_, _) | 
                            OpCode::Eqrr(_, _) => RegStatus::Bin,
                            _ => RegStatus::Any
                        };
                        tmp_instructions.push(Instruction {
                            opcode: inst.opcode.clone(),
                            target: Reg(n)
                        });
                    }
                }
                idx += 1;
            }
        }
    
        Self {
            start: 0,
            patches: targets
        }
    }

    pub fn deduplicate_patches(&mut self) {
        // remove duplications in patches
        let mut target_ids: Vec<_> = self.patches.keys().cloned().collect();
        target_ids.sort();
        for (id, next_id) in target_ids.iter().zip(target_ids.iter().skip(1)) {
            let this = &self.patches[id];
            let next = &self.patches[next_id];
            if id+this.insts.len() == next_id+next.insts.len() && this.jmp == next.jmp {
                let new_elmt = Patch {
                    insts: this.insts.iter().cloned().take(next_id-id).collect(),
                    jmp: Jmp::Static(*next_id)
                };
                *self.patches.get_mut(id).unwrap() = new_elmt;
            }
        }

    }

    pub fn remove_forwarding_patches(&mut self) {
        // remove patches that are only forwarding to other patches
        loop {
            // find patches that can be skipped (Jmp::Static && no instructions)
            let mut replace_elmts: std::collections::HashMap<usize, usize> = std::collections::HashMap::new();
            for k in self.patches.keys() {
                if let Patch { insts: v, jmp: Jmp::Static(n) } = &self.patches[k] {
                    if v.is_empty() {
                        replace_elmts.insert(*k, *n);
                    }
                }
            }
            
            // update target pointers in all patches
            for (_k, v) in self.patches.iter_mut() {
                for t in v.jmp.get_targets_mut() {
                    if let Some(e) = replace_elmts.get(t) {
                        *t = *e;
                    }
                }
            }
            
            // update start pointer
            if let Some(e) = replace_elmts.get(&self.start) {
                self.start = *e;
            }
    
            // check which patches are used
            let mut used_elmts = std::collections::HashSet::new();
            used_elmts.insert(self.start);
            for (_, v) in self.patches.iter() {
                for e in v.jmp.get_targets() {
                    used_elmts.insert(*e);
                }
            }
            
            // remove unused patches
            self.patches.retain(|k, _v| used_elmts.contains(k));
    
            // repeat the above until convergence
            if replace_elmts.is_empty() {
                break;
            }
        }
    }
}

impl std::fmt::Display for PatchProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut target_ids: Vec<_> = self.patches.keys().cloned().collect();
        target_ids.sort();
        writeln!(f, "PatchProgram ({} patches, start at {}) {{", target_ids.len(), self.start)?;
        for key in &target_ids {
            writeln!(f, "  Patch {} {{", key)?;
            for inst in &self.patches[&key].insts {
                writeln!(f, "    {}", inst)?;
            }
            writeln!(f, "    Jmp: {:?}", self.patches[&key].jmp)?;
            writeln!(f, "  }}")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}


use petgraph::prelude::*;
pub fn to_graph(program: &PatchProgram) -> Graph<String, String>{
    let mut graph = Graph::new();

    let start = graph.add_node("Start".to_string());
    let end = graph.add_node("End".to_string());

    let nodes = program.patches.iter().map(|(k, patch)| {
        let s: Vec<String> = patch.insts.iter().map(|x|format!("{}", x)).collect();
        let node = graph.add_node(s.join("\n"));
        (k, node)
    }).collect::<std::collections::HashMap<_, _>>();
    
    // add edge to start node
    graph.add_edge(start, nodes[&program.start], "".to_string());


    for (k, patch) in program.patches.iter() {
        /*for t in patch.jmp.get_targets() {
            graph.add_edge(nodes[&k], *nodes.get(t).unwrap_or(&end), "");
        }*/
        match &patch.jmp {
            Jmp::Static(n) => {
                graph.add_edge(nodes[&k], *nodes.get(&n).unwrap_or(&end), "".to_string());
            },
            Jmp::Cond(r, a, b) => {
                let node = graph.add_node(format!("< {} >", r));
                graph.add_edge(nodes[&k], node, "".to_string());
                graph.add_edge(node, *nodes.get(&a).unwrap_or(&end), "true".to_string());
                graph.add_edge(node, *nodes.get(&b).unwrap_or(&end), "false".to_string());
            },
            Jmp::Vec(r, v) => {
                let node = graph.add_node(format!("< {} >", r));
                graph.add_edge(nodes[&k], node, "".to_string());
                v.iter().enumerate().for_each(|(idx, t)| {
                    graph.add_edge(node, *nodes.get(&t).unwrap_or(&end), format!("{}", idx));
                });
            },
        };
    }

    graph
}

fn main() {
    let input = std::fs::read_to_string("00_orig.txt").unwrap();
    let program = Program::from_str(&input);
    println!("{}", program);
    let program_ip_inlined = program.inline_ip_lhs();
    println!("{}", program_ip_inlined);
    let mut patch_program = PatchProgram::from_program(program_ip_inlined);
    println!("{}", patch_program);
    patch_program.deduplicate_patches();
    println!("{}", patch_program);
    patch_program.remove_forwarding_patches();
    println!("{}", patch_program);
    let graph = to_graph(&patch_program);
    use petgraph::dot::*;
    std::fs::write("graph.dot", format!("{}", Dot::with_config(&graph, &[]))).unwrap();
}