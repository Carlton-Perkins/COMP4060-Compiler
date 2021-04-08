use itertools::Itertools;

use crate::{
    clang::LocalsInfo,
    common::{
        types::{Label, Variable},
        Graph,
    },
    xlang::{XArgument, XBlock, XInstruction, XProgram, XRegister, CALLER_SAVED_REGISTERS},
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
type XInterfenceGraph = Graph<LiveType>;
type XMoveGraph = Graph<LiveType>;
pub type Color = usize;
pub type ColorAssignment = HashMap<LiveType, Color>;

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

impl PartialOrd for LiveType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LiveType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (LiveType::Register(l), LiveType::Register(r)) => l.cmp(r),
            (LiveType::Register(_), LiveType::Var(_)) => std::cmp::Ordering::Less,
            (LiveType::Var(_), LiveType::Register(_)) => std::cmp::Ordering::Greater,
            (LiveType::Var(l), LiveType::Var(r)) => l.cmp(r),
        }
    }
}

fn observe(inst: &XInstruction) -> (LiveSet, LiveSet) {
    let (readset, writeset) = match inst {
        XInstruction::Addq(src, dst) => (set![src, dst], set![dst]),
        XInstruction::Subq(src, dst) => (set![src, dst], set![dst]),
        XInstruction::Movq(src, dst) => (set![src], set![dst]),
        XInstruction::Retq => (set![&XArgument::XReg(XRegister::RAX)], set![]),
        XInstruction::Negq(target) => (set![target], set![target]),
        XInstruction::Callq(_) => (set![], set![&XArgument::XReg(XRegister::RAX)]),
        XInstruction::Jmp(_) => (set![], set![]),
        XInstruction::Pushq(target) => (set![target], set![]),
        XInstruction::Popq(target) => (set![], set![target]),
        XInstruction::Xorq(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::Cmpq(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::Set(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::Movzbq(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::JmpIf(_, _) => {
            todo!("X0 -> X1")
        }
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
        XArgument::XBReg(_) => {
            todo!("X0 -> X1")
        }
    }
}

fn arg_set_to_live_set(ls: &HashSet<&XArgument>) -> LiveSet {
    ls.into_iter().filter_map(|x| arg_to_live(x)).collect()
}

pub fn build_interferences(blk: &XBlockLive, li: &LocalsInfo) -> (XInterfenceGraph, XMoveGraph) {
    let mut igraph = XInterfenceGraph::new();
    let mut mgraph = XMoveGraph::new();
    li.into_iter()
        .for_each(|x| igraph.add_vertex(&LiveType::Var(x.into())));
    blk.into_iter()
        .for_each(|(inst, liveafter)| build_graph(&inst, &liveafter, &mut igraph, &mut mgraph));
    (igraph, mgraph)
}

fn build_graph(
    inst: &XInstruction,
    liveafter: &LiveSet,
    g: &mut XInterfenceGraph,
    m: &mut XMoveGraph,
) {
    match inst {
        XInstruction::Addq(_, dst) => add_graph(g, liveafter, dst),
        XInstruction::Subq(_, dst) => add_graph(g, liveafter, dst),
        XInstruction::Movq(src, dst) => {
            let s = arg_to_live(src);
            let d = arg_to_live(dst).unwrap();
            if s.is_some() {
                let su = s.unwrap();
                g.add_vertex(&su.clone());
                m.update_edge(&su, &d)
            };
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
        XInstruction::Xorq(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::Cmpq(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::Set(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::Movzbq(_, _) => {
            todo!("X0 -> X1")
        }
        XInstruction::JmpIf(_, _) => {
            todo!("X0 -> X1")
        }
    }
}

fn add_graph(graph: &mut XInterfenceGraph, liveafter: &LiveSet, arg: &XArgument) {
    let d = arg_to_live(arg).unwrap();
    for v in liveafter {
        graph.update_edge(&d, v);
    }
}

pub fn color_graph(
    i_graph: &XInterfenceGraph,
    init_asn: &ColorAssignment,
    m_graph: &XMoveGraph,
) -> ColorAssignment {
    // Collect init assignment
    let mut asn = init_asn.clone();

    // Compute remaining variables that need assignement
    let mut vars_left: Vec<_> = i_graph
        .edges
        .keys()
        .filter(|x| !init_asn.contains_key(*x))
        .collect::<Vec<_>>()
        .into_iter()
        .sorted()
        .collect();

    // While there are variables left that need assigning
    while !vars_left.is_empty() {
        // println!("VarsLeft: {:?}", vars_left);
        // Ensure the list is always sorted by newest saturation information
        vars_left.sort_by_key(|x| sat(x, &asn, i_graph));

        // Take the largest saturation variable
        let var = vars_left.pop().unwrap();
        // println!("Took var {:?} with sat {}", var, sat(var, &asn, i_graph));

        // Compute the moves related to the current variable
        let moves = m_graph
            .edges
            .get(var)
            .unwrap_or(&set![])
            .into_iter()
            .filter(|x| asn.contains_key(x))
            .map(|x| asn.get(x).unwrap())
            .cloned()
            .collect();

        // Compute the color conflicts with the current variable
        let conflicts = color_conflicts(var, &asn, i_graph);

        // Find a valid color
        // Search the moves, then the conflicts
        for i in [moves, (0..=conflicts.len()).collect::<Vec<_>>()].concat() {
            // If i does not conflict
            if !conflicts.contains(&i) {
                // Then assign it color i
                // println!("Assigned {:?} color {}", var, i);
                asn.insert(var.clone(), i);
                break;
            }
        }
    }
    asn
}

fn sat(v: &LiveType, current_asn: &ColorAssignment, i_graph: &XInterfenceGraph) -> usize {
    i_graph
        .edges
        .get(v)
        .unwrap()
        .into_iter()
        .filter(|x| current_asn.contains_key(x))
        .count()
}

fn color_conflicts(
    v: &LiveType,
    current_asn: &ColorAssignment,
    i_graph: &XInterfenceGraph,
) -> HashSet<Color> {
    i_graph
        .edges
        .get(v)
        .unwrap()
        .into_iter()
        .filter(|x| current_asn.contains_key(x))
        .map(|x| current_asn.get(x).unwrap())
        .cloned()
        .collect()
}

#[cfg(test)]
mod test_uncover_live {
    use super::{LiveType::*, *};
    use crate::xlang::{XArgument::*, XInstruction::*, XRegister::*, ALL_REGISTERS};
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
        let tests: Vec<(XBlock, XInterfenceGraph, XMoveGraph)> = vec![
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
                XMoveGraph::from_edges(&[
                    (Var(Var!("v")), Var(Var!("x"))),
                    (Var(Var!("x")), Var(Var!("z"))),
                    (Var(Var!("x")), Var(Var!("y"))),
                    (Var(Var!("y")), Var(Var!("t"))),
                    (Var(Var!("z")), Register(RAX)),
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
                XMoveGraph::from_edges(&[]),
            ),
        ];

        for (start, expectedigraph, expectedmgraph) in tests {
            println!("Program: {:?}", start);
            println!("ExpectedIGraph: {:?}", expectedigraph);
            println!("ExpectedMGraph: {:?}", expectedmgraph);
            let live_set = start.uncover_live();
            let (interfere, moves) = build_interferences(&live_set, &LocalsInfo::new());
            assert_eq!(interfere.edges, expectedigraph.edges);
            assert_eq!(moves.edges, expectedmgraph.edges);
        }
    }

    #[test]
    fn test_color_graph() {
        let tests: Vec<(XInterfenceGraph, XMoveGraph, ColorAssignment)> = vec![(
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
            XMoveGraph::from_edges(&[
                (Var(Var!("v")), Var(Var!("x"))),
                (Var(Var!("x")), Var(Var!("z"))),
                (Var(Var!("x")), Var(Var!("y"))),
                (Var(Var!("y")), Var(Var!("t"))),
                (Var(Var!("z")), Register(RAX)),
            ]),
            vec![
                (Var(Var!("v")), 0),
                (Var(Var!("x")), 0),
                (Var(Var!("z")), 0),
                (Var(Var!("y")), 1),
                (Var(Var!("w")), 2),
                (Var(Var!("t")), 1),
                (Register(RAX), 0),
                (Register(RBX), 1),
                (Register(RCX), 2),
                (Register(RDX), 3),
                (Register(RSI), 4),
                (Register(RDI), 5),
                (Register(RBP), 6),
                (Register(RSP), 7),
                (Register(R8), 8),
                (Register(R9), 9),
                (Register(R10), 10),
                (Register(R11), 11),
                (Register(R12), 12),
                (Register(R13), 13),
                (Register(R14), 14),
                (Register(R15), 15),
            ]
            .iter()
            .cloned()
            .collect::<ColorAssignment>(),
        )];

        for (ig, mg, expected_ca) in tests {
            let init_asn = ALL_REGISTERS
                .into_iter()
                .enumerate()
                .map(|(e, r)| (LiveType::Register(*r), e))
                .collect::<ColorAssignment>();
            let ca = color_graph(&ig, &init_asn, &mg);

            println!("{:?}", ca);
            println!("{:?}", expected_ca);

            for (t, ec) in expected_ca.clone() {
                let c = ca.get(&t);
                match c {
                    Some(v) => {
                        if *v == ec {
                            println!("{:?}\t gave {:?} \t\t == {}", t, *v, ec);
                        } else {
                            println!("{:?}\t gave {:?} \t\t != {}", t, *v, ec);
                        }
                    }
                    None => {
                        println!("{:?}\t gave {:?}\t\t != {}", t, c, ec);
                    }
                }
            }

            assert_eq!(expected_ca, ca);
        }
    }
}
