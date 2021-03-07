use crate::{
    common::types::{Label, Variable},
    xlang::{XBlock, XInstruction, XProgram, XRegister},
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
        todo!()
    }
}

impl UncoverLive for XBlock {
    type LiveOut = XBlockLive;

    fn uncover_live(&self) -> Self::LiveOut {
        todo!()
    }
}

#[cfg(test)]
mod test_uncover_live {
    use super::{LiveType::*, *};
    use crate::xlang::{XArgument::*, XInstruction::*, XRegister::*};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_uncover_live() {
        let tests: Vec<(XBlock, XBlockLive)> = vec![
            (
                vec![Movq(Con(5), Reg(RAX)), Retq],
                vec![
                    (Movq(Con(5), Reg(RAX)), set![]),
                    (Retq, set![Register(RAX)]),
                ],
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
                    (Movq(Con(5), Reg(RAX)), set![]),
                    (Movq(Con(6), Reg(R9)), set![Register(RAX), Register(R9)]),
                    (Pushq(Reg(R9)), set![Register(RAX), Register(R9)]),
                    (Popq(Reg(RAX)), set![Register(RAX)]),
                    (Retq, set![Register(RAX)]),
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
                    (Movq(Con(5), Reg(RAX)), set![]),
                    (Movq(Con(6), Reg(R9)), set![Register(RAX)]),
                    (Addq(Reg(R9), Reg(RAX)), set![Register(RAX), Register(R9)]),
                    (Retq, set![Register(RAX)]),
                ],
            ),
        ];

        for (start, expected) in tests {
            let live_set = start.uncover_live();
            assert_eq!(live_set, expected);
        }
    }
}
