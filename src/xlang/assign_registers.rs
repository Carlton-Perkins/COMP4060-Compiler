use crate::{
    clang::LocalsInfo,
    common::types::{Label, Number},
    xlang::{
        Allocator, XArgument, XArgument::*, XBlock, XInstruction, XInstruction::*, XProgram,
        XRegister::*, CALLEE_SAVED_REGISTERS,
    },
};
use std::collections::HashMap;

pub trait AssignRegisters {
    fn asn_registers(&self, linfo: &LocalsInfo, alloc: impl Allocator) -> XProgram;
}

trait Asn {
    type RetT;
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self::RetT;
}

impl AssignRegisters for XProgram {
    fn asn_registers(&self, linfo: &LocalsInfo, alloc: impl Allocator) -> XProgram {
        let allocation = alloc.allocate(self, linfo);
        // println!("alloc: {:?}", allocation);
        let renames = allocation.variable_mapping;
        let stack_space = allocation.stack_space;
        let calle_save: XBlock = CALLEE_SAVED_REGISTERS
            .into_iter()
            .map(|r| Pushq(XReg(r.clone())))
            .collect();
        let calle_restore: XBlock = CALLEE_SAVED_REGISTERS
            .into_iter()
            .map(|r| Popq(XReg(r.clone())))
            .rev()
            .collect();
        let new_main = (
            "main".to_string(),
            vec![
                calle_save,
                vec![
                    Movq(XReg(RSP), XReg(RBP)),
                    Subq(XCon(stack_space as Number), XReg(RSP)),
                    Jmp(Label!("body")),
                ],
            ]
            .concat(),
        );
        let new_end = (
            "end".to_string(),
            vec![
                vec![
                    Movq(XReg(RAX), XReg(RDI)),
                    Callq(Label!("_print_int")),
                    Addq(XCon(stack_space as Number), XReg(RSP)),
                ],
                calle_restore,
                vec![Retq],
            ]
            .concat(),
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
    type RetT = Self;
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self::RetT {
        self.into_iter().filter_map(|x| x.asn(renames)).collect()
    }
}

impl Asn for XInstruction {
    type RetT = Option<Self>;
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self::RetT {
        match self {
            Addq(lh, rh) => Some(Addq(lh.asn(renames), rh.asn(renames))),
            Subq(lh, rh) => Some(Subq(lh.asn(renames), rh.asn(renames))),
            Movq(lh, rh) => {
                let lh_asn = lh.asn(renames);
                let rh_asn = rh.asn(renames);
                if lh_asn == rh_asn {
                    None
                } else {
                    Some(Movq(lh_asn, rh_asn))
                }
            }
            Retq => Some(Retq),
            Negq(v) => Some(Negq(v.asn(renames))),
            Callq(l) => Some(Callq(l.clone())),
            Jmp(l) => Some(Jmp(l.clone())),
            Pushq(v) => Some(Pushq(v.asn(renames))),
            Popq(v) => Some(Popq(v.asn(renames))),
        }
    }
}

impl Asn for XArgument {
    type RetT = Self;
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self::RetT {
        match self {
            XCon(c) => XCon(*c),
            XReg(r) => XReg(r.clone()),
            XDeref(r, offset) => XDeref(r.clone(), offset.clone()),
            XVar(v) => renames
                .get(v)
                .expect(
                    format!(
                        "Variable {} does not exist in the rename set {:?}",
                        v, renames
                    )
                    .as_str(),
                )
                .clone(),
        }
    }
}

#[cfg(test)]
mod test_assign_homes {
    use super::*;
    use crate::xlang::{GraphAllocator, StupidStackAllocator, XInterpMut};
    // use pretty_assertions::assert_eq;

    #[test]
    fn test_assign_homes_sstackalloc() {
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
                        Pushq(XReg(RBX)),
                        Pushq(XReg(RBP)),
                        Pushq(XReg(R12)),
                        Pushq(XReg(R13)),
                        Pushq(XReg(R14)),
                        Pushq(XReg(R15)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(16), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(16), XReg(RSP)),
                        Popq(XReg(R15)),
                        Popq(XReg(R14)),
                        Popq(XReg(R13)),
                        Popq(XReg(R12)),
                        Popq(XReg(RBP)),
                        Popq(XReg(RBX)),
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
                        Pushq(XReg(RBX)),
                        Pushq(XReg(RBP)),
                        Pushq(XReg(R12)),
                        Pushq(XReg(R13)),
                        Pushq(XReg(R14)),
                        Pushq(XReg(R15)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(32), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(32), XReg(RSP)),
                        Popq(XReg(R15)),
                        Popq(XReg(R14)),
                        Popq(XReg(R13)),
                        Popq(XReg(R12)),
                        Popq(XReg(RBP)),
                        Popq(XReg(RBX)),
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
            (
                (
                    XProgram!(XBlock!(
                        "main",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Movq(XReg(RAX), XVar!("0")),
                        Movq(XReg(RAX), XReg(RAX)), // Unneeded move should be removed
                        Movq(XVar!("0"), XReg(RAX)),
                    )),
                    vec!["0"].into_iter().map(|x| x.into()).collect(),
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBX)),
                        Pushq(XReg(RBP)),
                        Pushq(XReg(R12)),
                        Pushq(XReg(R13)),
                        Pushq(XReg(R14)),
                        Pushq(XReg(R15)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(16), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(16), XReg(RSP)),
                        Popq(XReg(R15)),
                        Popq(XReg(R14)),
                        Popq(XReg(R13)),
                        Popq(XReg(R12)),
                        Popq(XReg(RBP)),
                        Popq(XReg(RBX)),
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
        ];

        for ((prog, info), expect_prog) in tests {
            let prog_res = prog.interp();
            let expected_prog_res = expect_prog.interp();
            assert_eq!(prog_res, expected_prog_res);

            let asn = prog.asn_registers(&info, StupidStackAllocator {});
            let asn_res = asn.interp();
            assert_eq!(prog_res, asn_res);
            assert_eq!(asn, expect_prog);
        }
    }

    #[test]
    fn test_assign_homes_coloralloc() {
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
                        Pushq(XReg(RBX)),
                        Pushq(XReg(RBP)),
                        Pushq(XReg(R12)),
                        Pushq(XReg(R13)),
                        Pushq(XReg(R14)),
                        Pushq(XReg(R15)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(0), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(0), XReg(RSP)),
                        Popq(XReg(R15)),
                        Popq(XReg(R14)),
                        Popq(XReg(R13)),
                        Popq(XReg(R12)),
                        Popq(XReg(RBP)),
                        Popq(XReg(RBX)),
                        Retq
                    ),
                    XBlock!(
                        "body",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Jmp(Label!("end")),
                    )
                ),
            ),
            (
                (
                    XProgram!(XBlock!(
                        "main",
                        Movq(XCon(3), XVar!("0")),    //
                        Movq(XCon(2), XVar!("1")),    // 0
                        Movq(XVar!("1"), XVar!("2")), // 0,1
                        Addq(XVar!("0"), XVar!("2")), // 0,2
                        Movq(XVar!("2"), XReg(RAX)),  // RAX
                    )),
                    vec!["0", "1", "2"].into_iter().map(|x| x.into()).collect(),
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBX)),
                        Pushq(XReg(RBP)),
                        Pushq(XReg(R12)),
                        Pushq(XReg(R13)),
                        Pushq(XReg(R14)),
                        Pushq(XReg(R15)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(0), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(0), XReg(RSP)),
                        Popq(XReg(R15)),
                        Popq(XReg(R14)),
                        Popq(XReg(R13)),
                        Popq(XReg(R12)),
                        Popq(XReg(RBP)),
                        Popq(XReg(RBX)),
                        Retq
                    ),
                    XBlock!(
                        "body",
                        Movq(XCon(3), XReg(RBX)),
                        Movq(XCon(2), XReg(RAX)),
                        Addq(XReg(RBX), XReg(RAX)),
                        Jmp(Label!("end")),
                    )
                ),
            ),
            (
                (
                    // Move Bias test
                    XProgram!(XBlock!(
                        "main",
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
                    )),
                    vec!["v", "w", "x", "y", "t", "z"]
                        .into_iter()
                        .map(|x| x.into())
                        .collect(),
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBX)),
                        Pushq(XReg(RBP)),
                        Pushq(XReg(R12)),
                        Pushq(XReg(R13)),
                        Pushq(XReg(R14)),
                        Pushq(XReg(R15)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(0), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!(
                        "end",
                        Movq(XReg(RAX), XReg(RDI)),
                        Callq(Label!("_print_int")),
                        Addq(XCon(0), XReg(RSP)),
                        Popq(XReg(R15)),
                        Popq(XReg(R14)),
                        Popq(XReg(R13)),
                        Popq(XReg(R12)),
                        Popq(XReg(RBP)),
                        Popq(XReg(RBX)),
                        Retq
                    ),
                    XBlock!(
                        "body",
                        Movq(XCon(1), XReg(RAX)),
                        Movq(XCon(46), XReg(RCX)),
                        Addq(XCon(7), XReg(RAX)),
                        Movq(XReg(RAX), XReg(RBX)),
                        Addq(XCon(4), XReg(RBX)),
                        Addq(XReg(RCX), XReg(RAX)),
                        Negq(XReg(RBX)),
                        Addq(XReg(RBX), XReg(RAX)),
                        Retq,
                        Jmp(Label!("end")),
                    )
                ),
            ),
        ];

        for ((prog, info), expect_prog) in tests {
            let prog_res = prog.interp();
            let expected_prog_res = expect_prog.interp();
            assert_eq!(prog_res, expected_prog_res);

            let asn = prog.asn_registers(&info, GraphAllocator {});
            let asn_res = asn.interp();
            assert_eq!(prog_res, asn_res);
            assert_eq!(asn, expect_prog);
        }
    }
}
