use crate::{
    common::{
        types::{Label, Variable},
        Graph,
    },
    xlang::{XArgument, XBlock, XInstruction, XProgram, XRegister},
};
use std::collections::{HashMap, HashSet};

use super::xprog::CALLER_SAVED_REGISTERS;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum LiveType {
    Register(XRegister),
    Var(Variable),
}
type LiveSet = HashSet<LiveType>;
type XBlockLive = Vec<(XInstruction, LiveSet)>;
type XProgramLive = HashMap<Label, XBlockLive>;
type XInterfenceGraph = Graph<LiveType>;

impl PartialEq<XArgument> for LiveType {
    fn eq(&self, other: &XArgument) -> bool {
        match (self, other) {
            (LiveType::Register(lr), XArgument::XReg(rr)) => lr == rr,
            (LiveType::Register(lr), XArgument::XDeref(rr, _)) => lr == rr,
            (LiveType::Var(lv), XArgument::XVar(rv)) => lv == rv,
            _ => false,
        }
    }
}

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

            // println!("Inst: {:?}", inst);
            // println!("Readset: {:?}", readset);
            // println!("Writeset: {:?}", writeset);
            // println!("Liveafter: {:?}", liveafter);

            liveafter_set.push((inst.clone(), liveafter.clone()));
            liveafter = liveafter
                .difference(&writeset)
                .map(|x| x.clone())
                .collect::<LiveSet>()
                .union(&readset)
                .map(|x| x.clone())
                .collect();
            // println!("Livebefore: {:?}\n", liveafter);
        }

        // println!("---");

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
        XInstruction::Retq => (set![&XArgument::XReg(XRegister::RAX)], set![]),
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
        XArgument::XCon(_) => None,
        XArgument::XReg(r) => Some(LiveType::Register(r.clone())),
        XArgument::XDeref(r, _) => Some(LiveType::Register(r.clone())),
        XArgument::XVar(v) => Some(LiveType::Var(v.clone())),
    }
}

fn arg_set_to_live_set(ls: &HashSet<&XArgument>) -> LiveSet {
    ls.into_iter().filter_map(|x| arg_to_live(x)).collect()
}

fn build_interferences(blk: XBlockLive) -> XInterfenceGraph {
    let mut igraph = XInterfenceGraph::new();
    blk.into_iter()
        .for_each(|(inst, liveafter)| build_graph(&inst, &liveafter, &mut igraph));
    igraph
}

fn build_graph(inst: &XInstruction, liveafter: &LiveSet, g: &mut XInterfenceGraph) {
    match inst {
        XInstruction::Addq(_, dst) => add_graph(g, liveafter, dst),
        XInstruction::Subq(_, dst) => add_graph(g, liveafter, dst),
        XInstruction::Movq(src, dst) => {
            let d = arg_to_live(dst).unwrap();
            for v in liveafter {
                if v != dst && v != src {
                    g.update_edge(&d, v);
                }
            }
        }
        XInstruction::Retq => {}
        XInstruction::Negq(dst) => add_graph(g, liveafter, dst),
        XInstruction::Callq(_) => {
            for r in CALLER_SAVED_REGISTERS {
                for v in liveafter {
                    g.update_edge(v, &LiveType::Register(*r));
                }
            }
        }
        XInstruction::Jmp(_) => {}
        XInstruction::Pushq(_) => {}
        XInstruction::Popq(dst) => add_graph(g, liveafter, dst),
    }
}

fn add_graph(graph: &mut XInterfenceGraph, liveafter: &LiveSet, arg: &XArgument) {
    let d = arg_to_live(arg).unwrap();
    for v in liveafter {
        graph.update_edge(&d, v);
    }
}

#[cfg(test)]
mod test_uncover_live {
    use super::{LiveType::*, *};
    use crate::xlang::{XArgument::*, XInstruction::*, XRegister::*};
    use pretty_assertions::assert_eq;

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
                    Movq(XCon(1), XVar!("v")),
                    Movq(XCon(46), XVar!("w")),
                    Movq(XVar!("v"), XVar!("x")),
                    Addq(XCon(7), XVar!("x")),
                    Movq(XVar!("x"), XVar!("y")),
                    Addq(XCon(4), XVar!("y")),
                    Movq(XVar!("x"), XVar!("z")),
                    Addq(XVar!("w"), XVar!("z")),
                    Movq(XVar!("y"), XVar!("t")),
                    Negq(XVar!("t")),
                    Movq(XVar!("z"), XReg(RAX)),
                    Addq(XVar!("t"), XReg(RAX)),
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
                vec![Movq(XCon(5), XReg(RAX)), Retq],
                vec![set![Register(RAX)], set![Register(RAX)]],
            ),
            (
                vec![
                    Movq(XCon(5), XReg(RAX)),
                    Movq(XCon(6), XReg(R9)),
                    Pushq(XReg(R9)),
                    Popq(XReg(RAX)),
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
                    Movq(XCon(5), XReg(RAX)),
                    Movq(XCon(6), XReg(R9)),
                    Addq(XReg(R9), XReg(RAX)),
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

    #[test]
    fn test_interfere_graph() {
        let tests: Vec<(XBlock, XInterfenceGraph)> = vec![
            (
                vec![
                    Movq(XCon(1), XVar!("v")),
                    Movq(XCon(46), XVar!("w")),
                    Movq(XVar!("v"), XVar!("x")),
                    Addq(XCon(7), XVar!("x")),
                    Movq(XVar!("x"), XVar!("y")),
                    Addq(XCon(4), XVar!("y")),
                    Movq(XVar!("x"), XVar!("z")),
                    Addq(XVar!("w"), XVar!("z")),
                    Movq(XVar!("y"), XVar!("t")),
                    Negq(XVar!("t")),
                    Movq(XVar!("z"), XReg(RAX)),
                    Addq(XVar!("t"), XReg(RAX)),
                    Retq,
                ],
                XInterfenceGraph::from_edges(&[
                    (Var(Var!("v")), Var(Var!("w"))),
                    (Var(Var!("w")), Var(Var!("y"))),
                    (Var(Var!("w")), Var(Var!("x"))),
                    (Var(Var!("w")), Var(Var!("z"))),
                    (Var(Var!("y")), Var(Var!("w"))),
                    (Var(Var!("y")), Var(Var!("x"))),
                    (Var(Var!("y")), Var(Var!("z"))),
                    (Var(Var!("x")), Var(Var!("w"))),
                    (Var(Var!("x")), Var(Var!("y"))),
                    (Var(Var!("z")), Var(Var!("w"))),
                    (Var(Var!("z")), Var(Var!("y"))),
                    (Var(Var!("z")), Var(Var!("t"))),
                    (Var(Var!("t")), Var(Var!("z"))),
                    (Var(Var!("t")), Register(RAX)),
                ]),
            ),
            (
                vec![
                    Movq(XCon(5), XReg(RAX)),
                    Movq(XCon(6), XReg(R9)),
                    Pushq(XReg(R9)),
                    Popq(XReg(RAX)),
                    Retq,
                ],
                XInterfenceGraph::from_edges(&[]),
            ),
        ];

        for (start, expected) in tests {
            println!("Program: {:?}", start);
            println!("Expected: {:?}", expected);
            let live_set = start.uncover_live();
            let interfere = build_interferences(live_set);
            assert_eq!(interfere.edges, expected.edges);
        }
    }
}
