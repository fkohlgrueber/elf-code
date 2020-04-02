
// WORK IN PROGRESS

use std::collections::{HashMap, HashSet};
use crate::part_1::{
    Reg,
    Instruction,
    OpCode
};

use crate::part_2::{
    PatchProgram
};

type Idx = usize;

pub struct GraphProgram {
    start: Idx,
    nodes: HashMap<Idx, GraphNode>
}

impl GraphProgram {
    pub fn from_patch_program(program: &PatchProgram) -> Self {
        let mut nodes = HashMap::new();
        let end_idx = usize::max_value();
        nodes.insert(end_idx, GraphNode::End);

        let start = program.start;
        // skip nodes that are only forwarding
        loop {
            
        }

        Self {
            start,
            nodes
        }
    }
}

pub enum GraphNode {
    Inst(Instruction<Reg, Reg>, Idx),
    End,
    Jmp(GraphJmp, GraphJmpTarget)
}

pub enum GraphJmp {
    Reg(Reg),
    Opcode(OpCode<Reg>)
}

pub enum GraphJmpTarget {
    Cond(Idx, Idx),
    Vec(Vec<Idx>)
}

