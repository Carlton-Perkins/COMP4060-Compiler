use itertools::Itertools;

use crate::{
    clang::LocalsInfo,
    common::{types::Variable, utils::is_even},
    xlang::{
        build_interferences, color_graph, ColorAssignment, LiveType, XArgument, XArgument::*,
        XProgram, XRegister::*, USEABLE_REGISTERS,
    },
};
use std::collections::HashMap;

use super::uncover_live::UncoverLive;

pub trait Allocator {
    fn allocate(self, blk: &XProgram, linfo: &LocalsInfo) -> Allocation;
}

#[derive(Debug)]
pub struct Allocation {
    pub variable_mapping: HashMap<Variable, XArgument>,
    pub stack_space: usize,
}

pub struct StupidStackAllocator {}

impl Allocator for StupidStackAllocator {
    fn allocate(self, _: &XProgram, linfo: &LocalsInfo) -> Allocation {
        let var_count = linfo.len();
        let stack_space = 8
            * (if is_even(var_count) {
                var_count
            } else {
                var_count + 1
            });
        let variable_mapping = linfo
            .into_iter()
            .collect::<Vec<&String>>() // Need to ensure known ordering, so sort first
            .into_iter()
            .sorted_by_key(|x| x.clone())
            .enumerate()
            .map(|(idx, label)| (label.clone(), XDeref(RBP, (-8 * idx as isize) as i64)))
            .collect();

        Allocation {
            variable_mapping,
            stack_space,
        }
    }
}

pub struct GraphAllocator {}

impl Allocator for GraphAllocator {
    fn allocate(self, prog: &XProgram, li: &LocalsInfo) -> Allocation {
        assert!(prog.len() == 1); // this is going to end badly, but it should work for now

        // println!("Locals Info: {:?}", li);
        let livemap = prog.uncover_live();
        // println!("LiveMap: {:?}", livemap);
        let liveblk = livemap
            .into_iter()
            .map(|(_, blk)| blk)
            .take(1)
            .collect::<Vec<_>>()[0]
            .clone();
        // println!("Liveblk: {:?}", liveblk);
        // for (lab, live) in livemap {
        let (i_graph, m_graph) = build_interferences(&liveblk, &li);
        // println!("igraph: {:?}", i_graph);
        // println!("mgraph: {:?}", m_graph);
        let init_asn = USEABLE_REGISTERS
            .into_iter()
            .enumerate()
            .map(|(e, r)| (LiveType::Register(*r), e))
            .collect::<ColorAssignment>();
        // println!("init_asn: {:?}", init_asn);
        let color_asn = color_graph(&i_graph, &init_asn, &m_graph);
        // println!("coloring: {:?}", color_asn);
        let (var_asn, mut reg_asn) = color_asn
            .into_iter()
            .map(|(a, b)| match a {
                LiveType::Register(r) => (vec![], vec![(b, r)]),
                LiveType::Var(v) => (vec![(v, b)], vec![]),
            })
            .fold((vec![], vec![]), |(acc_a, acc_b), (a, b)| {
                ([acc_a, a].concat(), [acc_b, b].concat())
            });
        // println!("Varasn: {:?}", var_asn);
        // println!("Regasn: {:?}", reg_asn);
        reg_asn.sort_by_key(|(col, _)| col.clone());
        let reg_count = init_asn.len();
        let var_final_asn = var_asn
            .clone()
            .into_iter()
            .map(|(var, color)| {
                (
                    var,
                    match reg_asn.get(color) {
                        Some(r) => XArgument::XReg(r.1),
                        None => XArgument::XDeref(RBP, (color as i64 - reg_count as i64) * -8),
                    },
                )
            })
            .collect();

        let stack_size = var_asn
            .into_iter()
            .filter(|(_, color)| color >= &reg_count)
            .count();
        let stack_space = (stack_size * 8) + (if (stack_size * 8) % 16 != 0 { 8 } else { 0 });

        // }
        Allocation {
            variable_mapping: var_final_asn,
            stack_space,
        }
    }
}
