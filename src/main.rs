


pub mod part_1;
pub mod part_2;
pub mod part_3;

use part_1::{
    Program
};

use part_2::{
    PatchProgram,
    RegStatus,
};

fn main() {
    let input = std::fs::read_to_string("input19.txt").unwrap();
    let program = Program::from_str(&input);
    println!("{}", program);
    let program_ip_inlined = program.inline_ip_lhs();
    println!("{}", program_ip_inlined);
    let mut patch_program = PatchProgram::from_program(program_ip_inlined);
    println!("{}", patch_program);
    patch_program.remove_unreachable_branches(vec![RegStatus::Bin; 6]);
    println!("{}", patch_program);
    patch_program.deduplicate_patches();
    println!("{}", patch_program);
    patch_program.remove_forwarding_patches();
    println!("{}", patch_program);
    let graph = patch_program.to_graph();
    use petgraph::dot::*;
    std::fs::write("graph.dot", format!("{}", Dot::with_config(&graph, &[]))).unwrap();
}