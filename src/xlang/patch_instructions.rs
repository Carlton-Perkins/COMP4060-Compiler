use super::XInstruction;
use crate::xlang::{XArgument::*, XBlock, XInstruction::*, XProgram, TEMP_REGISTER};
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
        match self {
            Addq(XDeref(lr, lo), XDeref(rr, ro)) => {
                let src = XDeref(lr.clone(), lo.clone());
                let dst = XDeref(rr.clone(), ro.clone());
                vec![
                    Movq(src.clone(), XReg(TEMP_REGISTER)),
                    Addq(XReg(TEMP_REGISTER), dst.clone()),
                ]
            }
            Subq(XDeref(lr, lo), XDeref(rr, ro)) => {
                let src = XDeref(lr.clone(), lo.clone());
                let dst = XDeref(rr.clone(), ro.clone());
                vec![
                    Movq(src.clone(), XReg(TEMP_REGISTER)),
                    Subq(XReg(TEMP_REGISTER), dst.clone()),
                ]
            }
            Movq(XDeref(lr, lo), XDeref(rr, ro)) => {
                let src = XDeref(lr.clone(), lo.clone());
                let dst = XDeref(rr.clone(), ro.clone());
                vec![
                    Movq(src.clone(), XReg(TEMP_REGISTER)),
                    Movq(XReg(TEMP_REGISTER), dst.clone()),
                ]
            }
            _ => vec![self.clone()],
        }
    }
}

#[cfg(test)]
mod test_patch_instructions {
    use super::*;
    use crate::xlang::{XInterpMut, XRegister::*};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_patch() {
        let tests: Vec<(XProgram, XProgram)> = vec![
            (
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBP)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(16), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(XCon(16), XReg(RSP)), Popq(XReg(RBP)), Retq),
                    XBlock!(
                        "body",
                        Callq(Label!("_read_int")),
                        Callq(Label!("_read_int")),
                        Movq(XReg(RAX), XDeref(RBP, 0)),
                        Movq(XDeref(RBP, 0), XReg(RAX)),
                        Jmp(Label!("end")),
                    )
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBP)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(16), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(XCon(16), XReg(RSP)), Popq(XReg(RBP)), Retq),
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
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBP)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(32), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(XCon(32), XReg(RSP)), Popq(XReg(RBP)), Retq),
                    XBlock!(
                        "body",
                        Movq(XCon(3), XDeref(RBP, 0)),
                        Movq(XCon(2), XDeref(RBP, 8)),
                        Movq(XDeref(RBP, 8), XDeref(RBP, 16)),
                        Addq(XDeref(RBP, 0), XDeref(RBP, 16)),
                        Movq(XDeref(RBP, 16), XReg(RAX)),
                        Jmp(Label!("end")),
                    )
                ),
                XProgram!(
                    XBlock!(
                        "main",
                        Pushq(XReg(RBP)),
                        Movq(XReg(RSP), XReg(RBP)),
                        Subq(XCon(32), XReg(RSP)),
                        Jmp(Label!("body"))
                    ),
                    XBlock!("end", Addq(XCon(32), XReg(RSP)), Popq(XReg(RBP)), Retq),
                    XBlock!(
                        "body",
                        Movq(XCon(3), XDeref(RBP, 0)),
                        Movq(XCon(2), XDeref(RBP, 8)),
                        Movq(XDeref(RBP, 8), XReg(RAX)), // Move to temp first
                        Movq(XReg(RAX), XDeref(RBP, 16)),
                        Movq(XDeref(RBP, 0), XReg(RAX)), // Move to temp first
                        Addq(XReg(RAX), XDeref(RBP, 16)),
                        Movq(XDeref(RBP, 16), XReg(RAX)),
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
