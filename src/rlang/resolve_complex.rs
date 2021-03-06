use super::RExpr::*;
use crate::{
    common::types::Variable,
    rlang::{RExpr, RProgram},
};
use std::collections::HashMap;

type ProgramSeq = Vec<(Variable, RExpr)>;

#[derive(Clone)]
pub struct RCEnv {
    lifts: ProgramSeq,
    renames: HashMap<Variable, RExpr>,
}

pub trait ResolveComplex {
    fn resolve_complex(&self) -> Self;
    fn resolve_complex_(&self, env: &mut RCEnv) -> Self;
}

impl RCEnv {
    fn new() -> Self {
        RCEnv {
            lifts: ProgramSeq::new(),
            renames: HashMap::new(),
        }
    }
}

impl ResolveComplex for RProgram {
    fn resolve_complex(&self) -> Self {
        let mut env = RCEnv::new();
        let expr = self.resolve_complex_(&mut env);

        match env.lifts.len() {
            0 => expr,
            _ => recompose_lifts(&env.lifts),
        }
    }

    fn resolve_complex_(&self, mut env: &mut RCEnv) -> Self {
        match self {
            RNum(_) => self.clone(),
            RRead => lift(self, &mut env),
            RNegate(ex) => {
                let ex_rco = ex.resolve_complex_(&mut env);
                lift(&RNegate(Box::new(ex_rco)), &mut env)
            }
            RAdd(lh, rh) => {
                let lh_rco = lh.resolve_complex_(&mut env);
                let rh_rco = rh.resolve_complex_(&mut env);

                lift(&RAdd(Box::new(lh_rco), Box::new(rh_rco)), &mut env)
            }
            RLet(v, ve, be) => {
                let ve_rco = ve.resolve_complex_(&mut env);
                env.renames.insert(v.clone(), ve_rco);
                let br_rco = be.resolve_complex_(&mut env);

                lift(&br_rco, &mut env)
            }
            RVar(v) => env
                .renames
                .get(v)
                .expect(format!("RCO: Unbound var {:?}", v).as_str())
                .clone(),
        }
    }
}

fn recompose_lifts(seq: &ProgramSeq) -> RExpr {
    let split = seq.split_first().unwrap();

    match split {
        (first, rest) if rest.len() > 0 => {
            let (label, expr) = first;
            RLet(
                label.into(),
                Box::new(expr.clone()),
                Box::new(recompose_lifts(&rest.to_vec())),
            )
        }
        (first, _) => {
            let (label, expr) = first;
            RLet(
                label.into(),
                Box::new(expr.clone()),
                Box::new(RVar(label.into())),
            )
        }
    }
}

fn lift(expr: &RExpr, env: &mut RCEnv) -> RExpr {
    let nv: Variable = format!("r{}", env.lifts.len());
    env.lifts.push((nv.clone(), expr.clone()));
    RVar(nv)
}

#[cfg(test)]
mod test_rco {
    use super::*;
    use crate::{
        common::{traits::InterpMut, types::Number},
        rlang::Uniquify,
    };

    type Test = (RProgram, RProgram, Number);
    type Tests = Vec<Test>;

    #[test]
    fn test_rco() {
        let tests: Tests = vec![
            (RNum(5), RNum(5), 5),
            (RRead, RLet!("r0", RRead, RVar!("r0")), 0),
            (
                RNegate!(RNum(1)),
                RLet!("r0", RNegate!(RNum(1)), RVar!("r0")),
                -1,
            ),
            (
                RAdd!(RRead, RRead),
                RLet!(
                    "r0",
                    RRead,
                    RLet!(
                        "r1",
                        RRead,
                        RLet!("r2", RAdd!(RVar!("r0"), RVar!("r1")), RVar!("r2"))
                    )
                ),
                1,
            ),
            (
                RAdd(
                    Box::new(RAdd(Box::new(RNum(3)), Box::new(RNum(6)))),
                    Box::new(RNum(2)),
                ),
                RLet(
                    "r0".into(),
                    Box::new(RAdd(Box::new(RNum(3)), Box::new(RNum(6)))),
                    Box::new(RLet(
                        "r1".into(),
                        Box::new(RAdd(Box::new(RVar("r0".into())), Box::new(RNum(2)))),
                        Box::new(RVar("r1".into())),
                    )),
                ),
                11,
            ),
            (
                RAdd!(RAdd!(RNum(3), RNum(6)), RNum(2)),
                RLet!(
                    "r0",
                    RAdd!(RNum(3), RNum(6)),
                    RLet!("r1", RAdd!(RVar!("r0"), RNum(2)), RVar!("r1"))
                ),
                11,
            ),
            (
                RAdd!(RAdd!(RNum(3), RNum(6)), RNegate!(RAdd!(RRead, RNum(2)))),
                RLet!(
                    "r0",
                    RAdd!(RNum(3), RNum(6)),
                    RLet!(
                        "r1",
                        RRead,
                        RLet!(
                            "r2",
                            RAdd!(RVar!("r1"), RNum(2)),
                            RLet!(
                                "r3",
                                RNegate!(RVar!("r2")),
                                RLet!("r4", RAdd!(RVar!("r0"), RVar!("r3")), RVar!("r4"))
                            )
                        )
                    )
                ),
                7,
            ),
            (
                RAdd!(
                    RAdd!(RNum(2), RNum(3)),
                    RLet!("x", RRead, RAdd!(RVar!("x"), RVar!("x")))
                ),
                RLet!(
                    "r0",
                    RAdd!(RNum(2), RNum(3)),
                    RLet!(
                        "r1",
                        RRead,
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

        for (start, expected_rco, expected_res) in tests {
            println!("Running RCO {:?}", start);
            let start_res = start.interp();
            let uni = start.uniquify();
            let rco = uni.resolve_complex();
            let rco_res = rco.interp();

            assert_eq!(
                start_res, expected_res,
                "Start RProgram {:?} does not match expected result {} and returned {} instead",
                start, expected_res, start_res
            );
            assert_eq!(
                rco, expected_rco,
                "RCO'd program {:?} does not match expected program {:?}, Started with {:?}",
                rco, expected_rco, start
            );
            assert_eq!(
                rco_res, expected_res,
                "RCO'd program {:?} does not output the same result as the input program {:?}, {} != {}",
                rco, start, rco_res, expected_res
            );
        }
    }

    #[test]
    fn test_recompose_lifts() {
        let tests = vec![(
            vec![
                ("a".to_string(), RNum(4)),
                ("b".to_string(), RNum(4)),
                ("c".to_string(), RAdd!(RVar!("a"), RVar!("b"))),
            ],
            RLet!(
                "a",
                RNum(4),
                RLet!(
                    "b",
                    RNum(4),
                    RLet!("c", RAdd!(RVar!("a"), RVar!("b")), RVar!("c"))
                )
            ),
        )];

        for (start, expected) in tests {
            let res = recompose_lifts(&start);

            assert_eq!(
                res, expected,
                "Recompose produced incorrect result {:?}",
                start
            );
        }
    }
}
