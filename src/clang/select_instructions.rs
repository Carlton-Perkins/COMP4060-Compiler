use crate::{
    clang::{CArgument, CExpression, CProgram, CProgramInfo, CStatement, CTail},
    xlang::{xprog::XProgram, XArgument, XInstruction, XRegister},
};

trait SelectInstruction {
    fn select_instr(&self) -> XProgram;
}

impl SelectInstruction for CProgram {
    fn select_instr(&self) -> XProgram {
        todo!()
    }
}

#[cfg(test)]
mod test_select_instruction {
    use std::collections::HashMap;

    use super::*;
    use crate::clang::{CArgument::Num, CEnv, CExpression::*, CStatement::*, CTail::*};
    use crate::common::traits::{Interp, InterpMut};
    use crate::xlang::{XArgument::*, XEnv, XInstruction::*, XRegister::*};

    #[test]
    fn test_select_instr() {
        let tests: Vec<(CProgram, XProgram)> = vec![
            (
                CProgram!(CTail!(
                    "main",
                    CSeq!(CSet!("0", Arg(Num(5))), Return(CVar!("0")))
                )),
                XProgram!(XBlock!(
                    "main",
                    Movq(Con(5), XVar!("0")),
                    Movq(XVar!("0"), Reg(RAX)),
                    Retq
                )),
            ),
            (
                CProgram!(CTail!(
                    "main",
                    CSeq!(
                        CSet!("0", Arg(Num(3))),
                        CSeq!(
                            CSet!("1", Arg(Num(2))),
                            CSeq!(
                                CSet!("2", CExpression::Add(CVar!("0"), CVar!("1"))),
                                Return(CVar!("2"))
                            )
                        )
                    )
                )),
                XProgram!(XBlock!(
                    "main",
                    Movq(Con(3), XVar!("0")),
                    Movq(Con(2), XVar!("1")),
                    Addq(XVar!("0"), XVar!("1")),
                    Movq(XVar!("1"), Reg(RAX)),
                    Retq
                )),
            ),
            (
                CProgram!(CTail!(
                    "main",
                    CSeq!(
                        CSet!("0", Arg(Num(5))),
                        CSeq!(CSet!("1", Negate(CVar!("0"))), Return(CVar!("0")))
                    )
                )),
                XProgram!(XBlock!(
                    "main",
                    Movq(Con(5), XVar!("0")),
                    Negq(XVar!("0")),
                    Movq(XVar!("0"), Reg(RAX)),
                    Retq
                )),
            ),
            (
                CProgram!(CTail!("main", CSeq!(CSet!("0", Read), Return(CVar!("0"))))),
                XProgram!(XBlock!(
                    "main",
                    Callq(Label!("_read_int")),
                    Movq(Reg(RAX), XVar!("0")),
                    Movq(XVar!("0"), Reg(RAX)),
                    Retq
                )),
            ),
        ];

        for (cprog, expected_xprog) in tests {
            let c_res = cprog.interp(&mut CEnv::new(&cprog));
            let xprog = cprog.select_instr();
            let x_expected_res = expected_xprog.interp(&XEnv::new(HashMap::new()));
            let x_res = xprog.interp(&XEnv::new(HashMap::new()));

            assert_eq!(xprog, expected_xprog);
            assert_eq!(c_res, x_expected_res);
            assert_eq!(x_res, x_expected_res);
        }
    }
}
