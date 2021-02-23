use crate::{
    clang::{CArgument, CExpression, CProgram, CStatement, CTail},
    rlang::{RExpr, RProgram},
};

pub struct ECEnv {}

impl ECEnv {
    pub fn new() -> Self {
        ECEnv {}
    }
}

pub trait ExplicateControl {
    fn explicate_control(&self, env: ECEnv) -> CProgram;
}

impl ExplicateControl for RProgram {
    fn explicate_control(&self, _: ECEnv) -> CProgram {
        vec![(Label!("main"), econ_t(self))].into_iter().collect()
    }
}

fn econ_e(expr: &RExpr) -> CExpression {
    match expr {
        RExpr::Read => CExpression::Read(),
        RExpr::Negate(e) => CExpression::Negate(econ_a(e)),
        RExpr::Add(lh, rh) => CExpression::Add(econ_a(lh), econ_a(rh)),
        e => CExpression::Arg(econ_a(e)),
    }
}

fn econ_a(arg: &RExpr) -> CArgument {
    match arg {
        RExpr::Num(n) => CArgument::Num(*n),
        RExpr::Var(v) => CArgument::Var(v.clone()),
        _ => panic!("Econ: Invalid structure {:?}", arg),
    }
}

fn econ_t(tail: &RExpr) -> CTail {
    match tail {
        RExpr::Let(v, ve, be) => {
            CTail::Seq(CStatement::Set(v.clone(), econ_e(ve)), Box::new(econ_t(be)))
        }
        e => CTail::Return(econ_a(e)),
    }
}

#[cfg(test)]
mod test_econ {

    use super::*;
    use crate::{
        clang::CEnv,
        common::{traits::InterpMut, types::Number},
        rlang::REnv,
    };

    // Where RProgram is RCO'ed style
    type Test = (RProgram, Number);
    type Tests = Vec<Test>;

    #[test]
    fn test_econ() {
        let tests: Tests = vec![
            (RExpr::Num(5), 5),
            (Let!("r0", RExpr::Read, Var!("r0")), 0),
            (
                Let!(
                    "r0",
                    Add!(RExpr::Num(3), RExpr::Num(6)),
                    Let!("r1", Add!(Var!("r0"), RExpr::Num(2)), Var!("r1"))
                ),
                11,
            ),
            (
                Let!(
                    "r0",
                    Add!(RExpr::Num(2), RExpr::Num(3)),
                    Let!(
                        "r1",
                        RExpr::Read,
                        Let!(
                            "r2",
                            Add!(Var!("r1"), Var!("r1")),
                            Let!(
                                "r3",
                                Var!("r2"),
                                Let!("r4", Add!(Var!("r0"), Var!("r3")), Var!("r4"))
                            )
                        )
                    )
                ),
                5,
            ),
        ];

        for (rp, expected_res) in tests {
            println!("Econ: {:?}", rp);
            let rp_res = rp.interp(&mut REnv::new());
            let cp = rp.explicate_control(ECEnv::new());
            let cp_res = cp.interp(&mut CEnv::new(&cp));

            assert_eq!(rp_res, expected_res, "RProgram does not evaluate correctly");
            assert_eq!(
                cp_res, expected_res,
                "Generate CProgram does not evaluate correctly"
            );
        }
    }
}
