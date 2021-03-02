use crate::xlang::{XArgument::*, XBlock, XInstruction::*, XProgram, XRegister::*};

trait PatchInstructions {
    fn patch(&self) -> Self;
}

impl PatchInstructions for XProgram {
    fn patch(&self) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod test_patch_instructions {
    use super::*;
    use crate::xlang::{XEnv, XInterpMut};

    #[test]
    fn test_() {
        let tests: Vec<(XProgram, XProgram)> = vec![
            (
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(Reg(RBP)),
                        Movq(Reg(RSP), Reg(RBP)),
                        Subq(Con(16), Reg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(Con(16), Reg(RSP)), Popq(Reg(RBP)), Retq),
                    XBlock!(
                        "body",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Movq(Reg(RAX), Deref(RBP, 0)),
                        Movq(Deref(RBP, 0), Reg(RAX)),
                    )
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(Reg(RBP)),
                        Movq(Reg(RSP), Reg(RBP)),
                        Subq(Con(16), Reg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(Con(16), Reg(RSP)), Popq(Reg(RBP)), Retq),
                    XBlock!(
                        "body",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Movq(Reg(RAX), Deref(RBP, 0)),
                        Movq(Deref(RBP, 0), Reg(RAX)),
                    )
                ),
            ),
            (
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(Reg(RBP)),
                        Movq(Reg(RSP), Reg(RBP)),
                        Subq(Con(32), Reg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(Con(32), Reg(RSP)), Popq(Reg(RBP)), Retq),
                    XBlock!(
                        "body",
                        Movq(Con(3), Deref(RBP, 0)),
                        Movq(Con(2), Deref(RBP, 8)),
                        Movq(Deref(RBP, 8), Deref(RBP, 16)),
                        Addq(Deref(RBP, 0), Deref(RBP, 16)),
                        Movq(Deref(RBP, 16), Reg(RAX)),
                    )
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(Reg(RBP)),
                        Movq(Reg(RSP), Reg(RBP)),
                        Subq(Con(32), Reg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(Con(32), Reg(RSP)), Popq(Reg(RBP)), Retq),
                    XBlock!(
                        "body",
                        Movq(Con(3), Deref(RBP, 0)),
                        Movq(Con(2), Deref(RBP, 8)),
                        Movq(Deref(RBP, 8), Reg(RAX)), // Move to temp first
                        Movq(Reg(RAX), Deref(RBP, 16)),
                        Movq(Deref(RBP, 0), Reg(RAX)), // Move to temp first
                        Addq(Reg(RAX), Deref(RBP, 16)),
                        Movq(Deref(RBP, 16), Reg(RAX)),
                    )
                ),
            ),
        ];

        for (prog, expect_prog) in tests {
            let prog_res = prog.interp(&mut XEnv::new(&prog));
            let expected_prog_res = expect_prog.interp(&mut XEnv::new(&expect_prog));
            assert_eq!(prog_res, expected_prog_res);

            let patch = prog.patch();
            let patch_res = patch.interp(&mut XEnv::new(&patch));
            assert_eq!(prog_res, patch_res);
            assert_eq!(patch, expect_prog);
        }
    }
}
