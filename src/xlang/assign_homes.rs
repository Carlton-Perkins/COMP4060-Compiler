use crate::{
    clang::LocalsInfo,
    common::types::{Label, Number},
    xlang::{
        Allocator, StupidStackAllocator, XArgument, XArgument::*, XBlock, XInstruction,
        XInstruction::*, XProgram, XRegister::*,
    },
};
use std::collections::HashMap;

pub trait AssignHomes {
    fn asn_homes(&self, linfo: &LocalsInfo) -> XProgram;
}

trait Asn {
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self;
}

impl AssignHomes for XProgram {
    fn asn_homes(&self, linfo: &LocalsInfo) -> XProgram {
        let allocation = StupidStackAllocator::allocate(self, linfo);
        let renames = allocation.variable_mapping;
        let stack_space = allocation.stack_space;

        let new_main = XBlock!(
            "main",
            Pushq(XReg(RBP)),
            Movq(XReg(RSP), XReg(RBP)),
            Subq(XCon(stack_space as Number), XReg(RSP)),
            Jmp(Label!("body"))
        );
        let new_end = XBlock!(
            "end",
            Movq(XReg(RAX), XReg(RDI)),
            Callq(Label!("_print_int")),
            Addq(XCon(stack_space as Number), XReg(RSP)),
            Popq(XReg(RBP)),
            Retq
        );
        let body = self.get(&Label!("main")).unwrap();
        let new_body = (
            Label!("body"),
            [body.asn(&renames), vec![Jmp(Label!("end"))]].concat(),
        );

        XProgram!(new_main, new_body, new_end)
    }
}

impl Asn for XBlock {
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self {
        self.into_iter().map(|x| x.asn(renames)).collect()
    }
}

impl Asn for XInstruction {
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self {
        match self {
            Addq(lh, rh) => Addq(lh.asn(renames), rh.asn(renames)),
            Subq(lh, rh) => Subq(lh.asn(renames), rh.asn(renames)),
            Movq(lh, rh) => Movq(lh.asn(renames), rh.asn(renames)),
            Retq => Retq,
            Negq(v) => Negq(v.asn(renames)),
            Callq(l) => Callq(l.clone()),
            Jmp(l) => Jmp(l.clone()),
            Pushq(v) => Pushq(v.asn(renames)),
            Popq(v) => Popq(v.asn(renames)),
        }
    }
}

impl Asn for XArgument {
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self {
        match self {
            XCon(c) => XCon(*c),
            XReg(r) => XReg(r.clone()),
            XDeref(r, offset) => XDeref(r.clone(), offset.clone()),
            XVar(v) => renames.get(v).unwrap().clone(),
        }
    }
}

#[cfg(test)]
mod test_assign_homes {
    use super::*;
    use crate::xlang::XInterpMut;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_assign_homes() {
        let tests: Vec<((XProgram, LocalsInfo), XProgram)> = vec![
            (
                (
                    XProgram!(XBlock!(
                        "main",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Movq(XReg(RAX), XVar!("0")),
                        Movq(XVar!("0"), XReg(RAX)),
                    )),
                    vec!["0"].into_iter().map(|x| x.into()).collect(),
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBP)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(16), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(16), XReg(RSP)),
                        Popq(XReg(RBP)),
                        Retq
                    ),
                    XBlock!(
                        "body",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Movq(XReg(RAX), XDeref(RBP, 0)),
                        Movq(XDeref(RBP, 0), XReg(RAX)),
                        Jmp(Label!("end")),
                    )
                ),
            ),
            (
                (
                    XProgram!(XBlock!(
                        "main",
                        Movq(XCon(3), XVar!("0")),
                        Movq(XCon(2), XVar!("1")),
                        Movq(XVar!("1"), XVar!("2")),
                        Addq(XVar!("0"), XVar!("2")),
                        Movq(XVar!("2"), XReg(RAX)),
                    )),
                    vec!["0", "1", "2"].into_iter().map(|x| x.into()).collect(),
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBP)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(32), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(32), XReg(RSP)),
                        Popq(XReg(RBP)),
                        Retq
                    ),
                    XBlock!(
                        "body",
                        Movq(XCon(3), XDeref(RBP, 0)),
                        Movq(XCon(2), XDeref(RBP, -8)),
                        Movq(XDeref(RBP, -8), XDeref(RBP, -16)),
                        Addq(XDeref(RBP, 0), XDeref(RBP, -16)),
                        Movq(XDeref(RBP, -16), XReg(RAX)),
                        Jmp(Label!("end")),
                    )
                ),
            ),
        ];

        for ((prog, info), expect_prog) in tests {
            let prog_res = prog.interp();
            let expected_prog_res = expect_prog.interp();
            assert_eq!(prog_res, expected_prog_res);

            let asn = prog.asn_homes(&info);
            let asn_res = asn.interp();
            assert_eq!(prog_res, asn_res);
            assert_eq!(asn, expect_prog);
        }
    }
}
