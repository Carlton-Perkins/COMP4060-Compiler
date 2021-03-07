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
    fn explicate_control(&self) -> CProgram;
    fn explicate_control_(&self, env: ECEnv) -> CProgram;
}

impl ExplicateControl for RProgram {
    fn explicate_control_(&self, _: ECEnv) -> CProgram {
        vec![(Label!("main"), econ_t(self))].into_iter().collect()
    }

    fn explicate_control(&self) -> CProgram {
        self.explicate_control_(ECEnv::new())
    }
}

fn econ_e(expr: &RExpr) -> CExpression {
    match expr {
        RExpr::RRead => CExpression::Read,
        RExpr::RNegate(e) => CExpression::Negate(econ_a(e)),
        RExpr::RAdd(lh, rh) => CExpression::Add(econ_a(lh), econ_a(rh)),
        e => CExpression::Arg(econ_a(e)),
    }
}

fn econ_a(arg: &RExpr) -> CArgument {
    match arg {
        RExpr::RNum(n) => CArgument::Num(*n),
        RExpr::RVar(v) => CArgument::Var(v.clone()),
        _ => panic!("Econ: Invalid structure {:?}", arg),
    }
}

fn econ_t(tail: &RExpr) -> CTail {
    match tail {
        RExpr::RLet(v, ve, be) => {
            CTail::Seq(CStatement::Set(v.clone(), econ_e(ve)), Box::new(econ_t(be)))
        }
        e => CTail::Return(econ_a(e)),
    }
}

#[cfg(test)]
mod test_econ {
    use super::*;
    use crate::common::{traits::InterpMut, types::Number};
    use pretty_assertions::assert_eq;

    // Where RProgram is RCO'ed style
    type Test = (RProgram, Number);
    type Tests = Vec<Test>;

    #[test]
    fn test_econ() {
        let tests: Tests = vec![
            (RExpr::RNum(5), 5),
            (RLet!("r0", RExpr::RRead, RVar!("r0")), 0),
            (
                RLet!(
                    "r0",
                    RAdd!(RExpr::RNum(3), RExpr::RNum(6)),
                    RLet!("r1", RAdd!(RVar!("r0"), RExpr::RNum(2)), RVar!("r1"))
                ),
                11,
            ),
            (
                RLet!(
                    "r0",
                    RAdd!(RExpr::RNum(2), RExpr::RNum(3)),
                    RLet!(
                        "r1",
                        RExpr::RRead,
                        RLet!(
                            "r2",
                            RAdd!(RVar!("r1"), RVar!("r1")),
                            RLet!(
                                "r3",
                                RVar!("r2"),
                                RLet!("r4", RAdd!(RVar!("r0"), RVar!("r3")), RVar!("r4"))
                            )
                        )
                    )
                ),
                5,
            ),
        ];

        for (rp, expected_res) in tests {
            println!("Econ: {:?}", rp);
            let rp_res = rp.interp().unwrap();
            let cp = rp.explicate_control();
            let cp_res = cp.interp();

            assert_eq!(rp_res, expected_res, "RProgram does not evaluate correctly");
            assert_eq!(
                cp_res, expected_res,
                "Generate CProgram does not evaluate correctly"
            );
        }
    }
}
