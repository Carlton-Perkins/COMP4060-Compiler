use itertools::Itertools;

use crate::{
    clang::LocalsInfo,
    common::{types::Variable, utils::is_even},
    xlang::{XArgument, XArgument::*, XProgram, XRegister::*},
};
use std::collections::HashMap;

pub trait Allocator {
    fn allocate(self, blk: &XProgram, linfo: &LocalsInfo) -> Allocation;
}

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
