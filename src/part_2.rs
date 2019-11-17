use std::collections::{HashSet, HashMap};
use crate::part_1::{
    Reg,
    RegOrIp,
    Instruction,
    Program,
    OpCode
};
use petgraph::prelude::*;


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

impl Patch {
    pub fn get_free_and_modified_vars(&self) -> (HashSet<Reg>, HashSet<Reg>) {
        let mut free_vars: HashSet<Reg> = HashSet::new();
        let mut modified_vars: HashSet<Reg> = HashSet::new();
        for inst in &self.insts {
            inst.get_used_regs().into_iter().for_each(|r| if !modified_vars.contains(&r) {
                free_vars.insert(r);
            });
            modified_vars.insert(inst.target);
        }

        // check jmp as well
        if let Jmp::Cond(r, _, _) | Jmp::Vec(r, _) = &self.jmp {
            if !modified_vars.contains(r) {
                free_vars.insert(*r);
            }
        }

        (free_vars, modified_vars)
    }
}

pub struct PatchProgram {
    start: usize,
    patches: HashMap<usize, Patch>
}

impl PatchProgram {
    pub fn from_program(program: Program<Reg, RegOrIp>) -> Self {
        let insts = program.insts;
        let mut queue = vec!();
        let mut targets: HashMap<usize, Patch> = HashMap::new();
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
                                queue.push(i+1);
                                targets.insert(idx_orig, Patch{
                                    insts: tmp_instructions,
                                    jmp: Jmp::Static(i+1)
                                });
                            },
                            OpCode::Addi(Reg(r), i) => {
                                match regs[*r as usize] {
                                    RegStatus::Any => {
                                        for index in i+1..=insts.len() {
                                            queue.push(index);
                                        }
                                        targets.insert(idx_orig, Patch{
                                            insts: tmp_instructions,
                                            jmp: Jmp::Vec(Reg(*r), (i+1..=insts.len()).collect())
                                        });
                                    }
                                    RegStatus::Bin => {
                                        queue.push(i+1);
                                        queue.push(i+2);
                                        targets.insert(idx_orig, Patch{
                                            insts: tmp_instructions,
                                            jmp: Jmp::Cond(Reg(*r), i+2, i+1)
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
            let mut replace_elmts: HashMap<usize, usize> = HashMap::new();
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
    
            self.remove_unused_patches();
            
            // repeat the above until convergence
            if replace_elmts.is_empty() {
                break;
            }
        }
    }

    fn remove_unused_patches(&mut self) {
        // check which patches are used
        let mut used_elmts = HashSet::new();
        used_elmts.insert(self.start);
        for (_, v) in self.patches.iter() {
            for e in v.jmp.get_targets() {
                used_elmts.insert(*e);
            }
        }
        
        // remove unused patches
        self.patches.retain(|k, _v| used_elmts.contains(k));
    }

    pub fn remove_unreachable_branches(&mut self, mut regs: Vec<RegStatus>) {
        let mut curr_patch_idx = self.start;
        loop{ 
            for inst in &self.patches[&curr_patch_idx].insts {
                regs[inst.target.0 as usize] = match inst.opcode {
                    OpCode::Gtir(_, _) | 
                    OpCode::Gtri(_, _) | 
                    OpCode::Gtrr(_, _) | 
                    OpCode::Eqir(_, _) | 
                    OpCode::Eqri(_, _) | 
                    OpCode::Eqrr(_, _) => RegStatus::Bin,
                    _ => RegStatus::Any
                };
            }
            match &self.patches[&curr_patch_idx].jmp {
                Jmp::Static(n) => curr_patch_idx = *n,
                Jmp::Cond(_, _, _) => { break; },
                Jmp::Vec(r, v) => {
                    // if the register can only contain a binary value and the jump is Jmp::Vec,
                    // change it to a conditional jump
                    if let RegStatus::Bin = regs[r.0 as usize] {
                        self.patches.get_mut(&curr_patch_idx).unwrap().jmp = Jmp::Cond(*r, v[1], v[0]);
                    }
                    break;
                }
            }
        }
        self.remove_unused_patches();
    }

    pub fn get_data_dependencies(&self) -> HashMap<usize, Vec<HashSet<usize>>> {
        let mut hs: HashMap<usize, Vec<HashSet<usize>>> = HashMap::new();
        
        let init_hs = [100].iter().cloned().collect();
        let init_regs: Vec<HashSet<usize>> = vec!(init_hs; 6);  // fixme: number of registers should be part of the program structure
        let mut queue = vec!((self.start, init_regs));

        while let Some((curr_patch_idx, regs)) = queue.pop() {
            // update data dependencies based on `regs`. if the update doesn't cause changes, skip the rest of the loop
            if let Some(hs_regs) = hs.get_mut(&curr_patch_idx) {
                let hs_regs_orig = hs_regs.clone();
                for (hs_reg, reg) in hs_regs.iter_mut().zip(regs.iter()) {
                    hs_reg.extend(reg);
                }
                
                if &hs_regs_orig == hs_regs {
                    continue;
                }
            } else {
                hs.insert(curr_patch_idx, regs.clone());
            }

            // update regs
            if let Some(p) = self.patches.get(&curr_patch_idx) {
                let mut regs = hs.get(&curr_patch_idx).unwrap().clone();
                // for each reg that's modified within the patch, set the corresponding hashmap to the current patch idx
                p.get_free_and_modified_vars().1.iter().for_each(|r| {
                    let mut tmp_hs = HashSet::new();
                    tmp_hs.insert(curr_patch_idx);
                    regs[r.0 as usize] = tmp_hs;
                });

                for target in p.jmp.get_targets() {
                    queue.push((*target, regs.clone()));
                }
            }


        }
        
        hs
    }

    
    pub fn to_graph(&self) -> Graph<String, String>{
        let mut graph = Graph::new();

        let deps = self.get_data_dependencies();

        let used_by = |patch_id, mod_vars: Vec<Reg>| {
            mod_vars.iter().cloned().map(|mv| {
                deps.iter().filter_map(|(k, v)| if v[mv.0 as usize].contains(patch_id) { 
                    if self.patches.contains_key(&k) && self.patches[k].get_free_and_modified_vars().0.contains(&mv) {
                        Some(k)
                    } else { None }
                } else { None }).collect::<Vec<_>>()
            }).collect::<Vec<_>>()
        };

        let start = graph.add_node("Start".to_string());
        let end = graph.add_node("End".to_string());

        let nodes = self.patches.iter().map(|(k, patch)| {
            let mut s: Vec<String> = patch.insts.iter().map(|x| x.pretty_print()).collect();
            let (free_vars, mod_vars) = patch.get_free_and_modified_vars();
            let sorted = |hs: HashSet<Reg>| { let mut v = hs.into_iter().collect::<Vec<_>>(); v.sort(); v };
            s.push(format!("Free vars: {:?}\nModified vars: {:?}", sorted(free_vars.clone()), sorted(mod_vars.clone())));
            s.push(format!("Deps: {:?}", sorted(free_vars).iter().map(|r| &deps[k][r.0 as usize]).collect::<Vec<_>>()));
            s.push(format!("Used by: {:?}", used_by(k, sorted(mod_vars))));
            s.push(format!("Key: {}", k));
            let node = graph.add_node(s.join("\n"));
            (k, node)
        }).collect::<HashMap<_, _>>();
        
        // add edge to start node
        graph.add_edge(start, nodes[&self.start], "".to_string());


        for (k, patch) in self.patches.iter() {
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

