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
    fn allocate(self, prog: &XProgram, _: &LocalsInfo) -> Allocation {
        assert!(prog.len() == 1); // this is going to end badly, but it should work for now

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
        let (i_graph, m_graph) = build_interferences(liveblk);
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
        let var_final_asn = var_asn
            .into_iter()
            .map(|(var, color)| (var, XArgument::XReg(reg_asn.get(color).unwrap().1)))
            .collect();

        // }
        Allocation {
            variable_mapping: var_final_asn,
            stack_space: 0,
        }
    }
}
