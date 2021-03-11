use crate::{
    clang::{CArgument, CExpression, CProgram, CStatement, CTail},
    common::types::Label,
    xlang::{xprog::XProgram, XArgument, XBlock, XInstruction, XRegister::*},
};

pub trait SelectInstruction {
    fn select_instr(&self) -> XProgram;
}

impl SelectInstruction for CProgram {
    fn select_instr(&self) -> XProgram {
        self.into_iter().map(select_blk).collect()
    }
}

fn select_blk((label, blk): (&Label, &CTail)) -> (Label, XBlock) {
    (label.clone(), select_tail(blk))
}

fn select_tail(tail: &CTail) -> XBlock {
    match tail {
        CTail::Return(arg) => vec![XInstruction::Movq(select_arg(arg), XArgument::XReg(RAX))],
        CTail::Seq(stat, more) => [select_stmt(stat), select_tail(more)].concat(),
    }
}

fn select_arg(arg: &CArgument) -> XArgument {
    match arg {
        CArgument::Num(n) => XArgument::XCon(*n),
        CArgument::Var(v) => XArgument::XVar(v.into()),
    }
}

fn select_stmt(stat: &CStatement) -> XBlock {
    match stat {
        CStatement::Set(v, e) => select_expr(&XArgument::XVar(v.into()), e),
    }
}

fn select_expr(dst: &XArgument, src: &CExpression) -> XBlock {
    match src {
        CExpression::Arg(src_a) => vec![XInstruction::Movq(select_arg(src_a), dst.clone())],
        CExpression::Read => vec![
            XInstruction::Callq(Label!("_read_int")),
            XInstruction::Movq(XArgument::XReg(RAX), dst.clone()),
        ],
        CExpression::Negate(src_a) => vec![
            XInstruction::Movq(select_arg(src_a), dst.clone()),
            XInstruction::Negq(dst.clone()),
        ],
        CExpression::Add(lh, rh) => vec![
            XInstruction::Movq(select_arg(rh), dst.clone()),
            XInstruction::Addq(select_arg(lh), dst.clone()),
        ],
    }
}

#[cfg(test)]
mod test_select_instruction {
    use super::*;
    use crate::{
        clang::{CArgument::Num, CExpression::*, CTail::*},
        common::traits::InterpMut,
        xlang::{XArgument::*, XInstruction::*, XInterpMut},
    };
    use pretty_assertions::assert_eq;

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
                    Movq(XCon(5), XVar!("0")),
                    Movq(XVar!("0"), XReg(RAX)),
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
                    Movq(XCon(3), XVar!("0")),
                    Movq(XCon(2), XVar!("1")),
                    Movq(XVar!("1"), XVar!("2")),
                    Addq(XVar!("0"), XVar!("2")),
                    Movq(XVar!("2"), XReg(RAX)),
                )),
            ),
            (
                CProgram!(CTail!(
                    "main",
                    CSeq!(
                        CSet!("0", Arg(Num(5))),
                        CSeq!(CSet!("1", Negate(CVar!("0"))), Return(CVar!("1")))
                    )
                )),
                XProgram!(XBlock!(
                    "main",
                    Movq(XCon(5), XVar!("0")),
                    Movq(XVar!("0"), XVar!("1")),
                    Negq(XVar!("1")),
                    Movq(XVar!("1"), XReg(RAX)),
                )),
            ),
            (
                CProgram!(CTail!("main", CSeq!(CSet!("0", Read), Return(CVar!("0"))))),
                XProgram!(XBlock!(
                    "main",
                    Callq(Label!("_read_int")),
                    Movq(XReg(RAX), XVar!("0")),
                    Movq(XVar!("0"), XReg(RAX)),
                )),
            ),
        ];

        for (cprog, expected_xprog) in tests {
            let c_res = cprog.interp();
            let xprog = cprog.select_instr();
            let x_expected_res = expected_xprog.interp();
            let x_res = xprog.interp();

            assert_eq!(xprog, expected_xprog);
            assert_eq!(c_res, x_expected_res);
            assert_eq!(x_res, x_expected_res);
        }
    }
}
