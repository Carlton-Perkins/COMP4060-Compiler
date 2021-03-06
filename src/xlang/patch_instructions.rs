use crate::xlang::{XArgument::*, XBlock, XInstruction::*, XProgram, XRegister::*};

use super::XInstruction;
pub trait PatchInstructions {
    fn patch(&self) -> Self;
}

trait DoPatch: Sized {
    fn do_patch(&self) -> Vec<Self>;
}

impl PatchInstructions for XProgram {
    fn patch(&self) -> Self {
        self.into_iter()
            .map(|(lab, prog)| (lab.clone(), prog.patch()))
            .collect()
    }
}

impl PatchInstructions for XBlock {
    fn patch(&self) -> Self {
        self.into_iter()
            .map(|x| x.do_patch())
            .flatten()
            .map(|x| x.clone())
            .collect()
    }
}

impl DoPatch for XInstruction {
    fn do_patch(&self) -> Vec<Self> {
        let temp_reg = RAX;
        match self {
            Addq(Deref(lr, lo), Deref(rr, ro)) => {
                let src = Deref(lr.clone(), lo.clone());
                let dst = Deref(rr.clone(), ro.clone());
                vec![
                    Movq(src.clone(), Reg(temp_reg)),
                    Addq(Reg(temp_reg), dst.clone()),
                ]
            }
            Subq(Deref(lr, lo), Deref(rr, ro)) => {
                let src = Deref(lr.clone(), lo.clone());
                let dst = Deref(rr.clone(), ro.clone());
                vec![
                    Movq(src.clone(), Reg(temp_reg)),
                    Subq(Reg(temp_reg), dst.clone()),
                ]
            }
            Movq(Deref(lr, lo), Deref(rr, ro)) => {
                let src = Deref(lr.clone(), lo.clone());
                let dst = Deref(rr.clone(), ro.clone());
                vec![
                    Movq(src.clone(), Reg(temp_reg)),
                    Movq(Reg(temp_reg), dst.clone()),
                ]
            }
            _ => vec![self.clone()],
        }
    }
}

#[cfg(test)]
mod test_patch_instructions {
    use super::*;
    use crate::xlang::XInterpMut;

    #[test]
    fn test_patch() {
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
                        Jmp(Label!("end")),
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
                        Jmp(Label!("end")),
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
                        Jmp(Label!("end")),
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
                        Jmp(Label!("end")),
                    )
                ),
            ),
        ];

        for (prog, expect_prog) in tests {
            let prog_res = prog.interp();
            let expected_prog_res = expect_prog.interp();
            assert_eq!(prog_res, expected_prog_res);

            let patch = prog.patch();
            let patch_res = patch.interp();
            assert_eq!(prog_res, patch_res);
            assert_eq!(patch, expect_prog);
        }
    }
}
