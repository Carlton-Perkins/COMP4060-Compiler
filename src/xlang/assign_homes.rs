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
        let allocator = StupidStackAllocator::allocate(self, linfo);
        let renames = allocator.variable_mapping;
        let stack_space = allocator.stack_space;

        let new_main = XBlock!(
            "main",
            Pushq(Reg(RBP)),
            Movq(Reg(RSP), Reg(RBP)),
            Subq(Con(stack_space as Number), Reg(RSP)),
            Jmp(Label!("body"))
        );
        let new_end = XBlock!(
            "end",
            Movq(Reg(RAX), Reg(RDI)),
            Callq(Label!("_print_int")),
            Addq(Con(stack_space as Number), Reg(RSP)),
            Popq(Reg(RBP)),
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
            Con(c) => Con(*c),
            Reg(r) => Reg(r.clone()),
            Deref(r, offset) => Deref(r.clone(), offset.clone()),
            Var(v) => renames.get(v).unwrap().clone(),
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
                        Movq(Reg(RAX), XVar!("0")),
                        Movq(XVar!("0"), Reg(RAX)),
                    )),
                    vec!["0"].into_iter().map(|x| x.into()).collect(),
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(Reg(RBP)),
                        Movq(Reg(RSP), Reg(RBP)),
                        Subq(Con(16), Reg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(Reg(RAX), Reg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(Con(16), Reg(RSP)),
                        Popq(Reg(RBP)),
                        Retq
                    ),
                    XBlock!(
                        "body",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Movq(Reg(RAX), Deref(RBP, 0)),
                        Movq(Deref(RBP, 0), Reg(RAX)),
                        Jmp(Label!("end")),
                    )
                ),
            ),
            (
                (
                    XProgram!(XBlock!(
                        "main",
                        Movq(Con(3), XVar!("0")),
                        Movq(Con(2), XVar!("1")),
                        Movq(XVar!("1"), XVar!("2")),
                        Addq(XVar!("0"), XVar!("2")),
                        Movq(XVar!("2"), Reg(RAX)),
                    )),
                    vec!["0", "1", "2"].into_iter().map(|x| x.into()).collect(),
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(Reg(RBP)),
                        Movq(Reg(RSP), Reg(RBP)),
                        Subq(Con(32), Reg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(Reg(RAX), Reg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(Con(32), Reg(RSP)),
                        Popq(Reg(RBP)),
                        Retq
                    ),
                    XBlock!(
                        "body",
                        Movq(Con(3), Deref(RBP, 0)),
                        Movq(Con(2), Deref(RBP, -8)),
                        Movq(Deref(RBP, -8), Deref(RBP, -16)),
                        Addq(Deref(RBP, 0), Deref(RBP, -16)),
                        Movq(Deref(RBP, -16), Reg(RAX)),
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
