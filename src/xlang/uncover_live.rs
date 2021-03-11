use crate::{
    common::types::{Label, Variable},
    xlang::{XArgument, XBlock, XInstruction, XProgram, XRegister},
};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum LiveType {
    Register(XRegister),
    Var(Variable),
}
type LiveSet = HashSet<LiveType>;
type XBlockLive = Vec<(XInstruction, LiveSet)>;
type XProgramLive = HashMap<Label, XBlockLive>;

pub trait UncoverLive {
    type LiveOut;

    fn uncover_live(&self) -> Self::LiveOut;
}

impl UncoverLive for XProgram {
    type LiveOut = XProgramLive;

    fn uncover_live(&self) -> Self::LiveOut {
        self.into_iter()
            .map(|(lab, prog)| (lab.clone(), prog.uncover_live()))
            .collect()
    }
}

impl UncoverLive for XBlock {
    type LiveOut = XBlockLive;

    fn uncover_live(&self) -> Self::LiveOut {
        // Start at last instruction in block

        // Iterate though attaching the live set
        // Where liveafter = (liveafter(previous or mt) / writes) U reads

        let mut liveafter_set: XBlockLive = XBlockLive::new();

        let mut liveafter = LiveSet::new();
        liveafter.insert(LiveType::Register(XRegister::RAX));
        for inst in self.into_iter().rev() {
            let (readset, writeset) = observe(inst);

            println!("Inst: {:?}", inst);
            println!("Readset: {:?}", readset);
            println!("Writeset: {:?}", writeset);
            println!("Liveafter: {:?}", liveafter);

            liveafter_set.push((inst.clone(), liveafter.clone()));
            liveafter = liveafter
                .difference(&writeset)
                .map(|x| x.clone())
                .collect::<LiveSet>()
                .union(&readset)
                .map(|x| x.clone())
                .collect();
            println!("Livebefore: {:?}\n", liveafter);
        }

        println!("---");

        assert_eq!(liveafter, set![]);
        liveafter_set.reverse();
        liveafter_set
    }
}

fn observe(inst: &XInstruction) -> (LiveSet, LiveSet) {
    let (readset, writeset) = match inst {
        XInstruction::Addq(src, dst) => (set![src, dst], set![dst]),
        XInstruction::Subq(src, dst) => (set![src, dst], set![dst]),
        XInstruction::Movq(src, dst) => (set![src], set![dst]),
        XInstruction::Retq => (set![&XArgument::Reg(XRegister::RAX)], set![]),
        XInstruction::Negq(target) => (set![target], set![target]),
        XInstruction::Callq(_) => (set![], set![]),
        XInstruction::Jmp(_) => (set![], set![]),
        XInstruction::Pushq(target) => (set![target], set![]),
        XInstruction::Popq(target) => (set![], set![target]),
    };
    (
        arg_set_to_live_set(&readset),
        arg_set_to_live_set(&writeset),
    )
}

fn arg_to_live(arg: &XArgument) -> Option<LiveType> {
    match arg {
        XArgument::Con(_) => None,
        XArgument::Reg(r) => Some(LiveType::Register(r.clone())),
        XArgument::Deref(r, _) => Some(LiveType::Register(r.clone())),
        XArgument::Var(v) => Some(LiveType::Var(v.clone())),
    }
}

fn arg_set_to_live_set(ls: &HashSet<&XArgument>) -> LiveSet {
    ls.into_iter().filter_map(|x| arg_to_live(x)).collect()
}

#[cfg(test)]
mod test_uncover_live {
    use super::{LiveType::*, *};
    use crate::xlang::{XArgument::*, XInstruction::*, XRegister::*};
    // use pretty_assertions::assert_eq;

    macro_rules! LTVar {
        ($expr:expr) => {
            LiveType::Var($expr.into())
        };
    }

    #[test]
    fn test_uncover_live() {
        let tests: Vec<(XBlock, Vec<LiveSet>)> = vec![
            (
                vec![
                    Movq(Con(1), XVar!("v")),
                    Movq(Con(46), XVar!("w")),
                    Movq(XVar!("v"), XVar!("x")),
                    Addq(Con(7), XVar!("x")),
                    Movq(XVar!("x"), XVar!("y")),
                    Addq(Con(4), XVar!("y")),
                    Movq(XVar!("x"), XVar!("z")),
                    Addq(XVar!("w"), XVar!("z")),
                    Movq(XVar!("y"), XVar!("t")),
                    Negq(XVar!("t")),
                    Movq(XVar!("z"), Reg(RAX)),
                    Addq(XVar!("t"), Reg(RAX)),
                    Retq,
                ],
                vec![
                    set![LTVar!("v")],
                    set![LTVar!("v"), LTVar!("w")],
                    set![LTVar!("x"), LTVar!("w")],
                    set![LTVar!("x"), LTVar!("w")],
                    set![LTVar!("x"), LTVar!("w"), LTVar!("y")],
                    set![LTVar!("x"), LTVar!("w"), LTVar!("y")],
                    set![LTVar!("z"), LTVar!("w"), LTVar!("y")],
                    set![LTVar!("z"), LTVar!("y")],
                    set![LTVar!("z"), LTVar!("t")],
                    set![LTVar!("z"), LTVar!("t")],
                    set![LTVar!("t"), Register(RAX)],
                    set![Register(RAX)],
                ],
            ),
            (
                vec![Movq(Con(5), Reg(RAX)), Retq],
                vec![set![Register(RAX)], set![Register(RAX)]],
            ),
            (
                vec![
                    Movq(Con(5), Reg(RAX)),
                    Movq(Con(6), Reg(R9)),
                    Pushq(Reg(R9)),
                    Popq(Reg(RAX)),
                    Retq,
                ],
                vec![
                    set![],
                    set![Register(R9)],
                    set![],
                    set![Register(RAX)],
                    set![Register(RAX)],
                ],
            ),
            (
                vec![
                    Movq(Con(5), Reg(RAX)),
                    Movq(Con(6), Reg(R9)),
                    Addq(Reg(R9), Reg(RAX)),
                    Retq,
                ],
                vec![
                    set![Register(RAX)],
                    set![Register(RAX), Register(R9)],
                    set![Register(RAX)],
                    set![Register(RAX)],
                ],
            ),
        ];

        for (start, expected) in tests {
            println!("Program: {:?}", start);
            println!("Expected: {:?}", expected);
            let live_set = start.uncover_live();
            let live_set_striped = live_set.into_iter().map(|(_, set)| set);
            assert!(
                live_set_striped
                    .clone()
                    .zip(expected.clone().into_iter())
                    .map(|(a, b)| a == b)
                    .all(|x| x),
                "Program does not uncover correctly: Expected \n{:?}\n!=\n{:?}",
                expected,
                live_set_striped.collect::<Vec<LiveSet>>()
            );
        }
    }
}
