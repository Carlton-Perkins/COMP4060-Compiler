use super::{RExpr::*, CMP};
use crate::{
    common::types::Variable,
    rlang::{RExpr, RProgram},
};
use std::collections::HashMap;

type ProgramSeq = Vec<(Variable, RExpr)>;

#[derive(Clone, Debug)]
pub struct RCEnv {
    lifts: ProgramSeq,
    renames: HashMap<Variable, RExpr>,
    variable_counter: usize,
}

pub trait ResolveComplex {
    fn resolve_complex(&self) -> Self;
    fn resolve_complex_(&self, is_tail: bool, env: &mut RCEnv) -> Self;
}

impl RCEnv {
    fn new() -> Self {
        RCEnv {
            lifts: ProgramSeq::new(),
            renames: HashMap::new(),
            variable_counter: 0,
        }
    }
}

impl ResolveComplex for RProgram {
    fn resolve_complex(&self) -> Self {
        let mut env = RCEnv::new();
        resolve_expr(self, true, &mut env)
    }

    fn resolve_complex_(&self, is_tail: bool, mut env: &mut RCEnv) -> Self {
        match self {
            RNum(_) => self.clone(),
            RRead => lift(self, &mut env),
            RNegate(ex) => {
                let ex_rco = ex.resolve_complex_(false, &mut env);
                lift(&RNegate(Box::new(ex_rco)), &mut env)
            }
            RAdd(lh, rh) => {
                let lh_rco = lh.resolve_complex_(false, &mut env);
                let rh_rco = rh.resolve_complex_(false, &mut env);

                lift(&RAdd(Box::new(lh_rco), Box::new(rh_rco)), &mut env)
            }
            RLet(v, ve, be) => {
                let ve_rco = ve.resolve_complex_(false, &mut env);
                env.renames.insert(v.clone(), ve_rco);
                let br_rco = be.resolve_complex_(is_tail, &mut env);
                lift(&br_rco, &mut env)
            }
            RVar(v) => env
                .renames
                .get(v)
                .expect(format!("RCO: Unbound var {:?}", v).as_str())
                .clone(),
            RBool(_) => self.clone(),
            RCmp(cmp, lh, rh) => {
                let lh_rco = lh.resolve_complex_(false, &mut env);
                let rh_rco = rh.resolve_complex_(false, &mut env);

                lift(&RCmp(*cmp, Box::new(lh_rco), Box::new(rh_rco)), &mut env)
            }
            RIf(cond, tb, fb) => {
                let mut tb_env = RCEnv {
                    lifts: vec![],
                    renames: HashMap::new(),
                    variable_counter: env.variable_counter,
                };
                let tb_rco = resolve_expr(tb, is_tail, &mut tb_env);
                let mut fb_env = RCEnv {
                    lifts: vec![],
                    renames: HashMap::new(),
                    variable_counter: tb_env.variable_counter,
                };
                let fb_rco = resolve_expr(fb, is_tail, &mut fb_env);
                println!("tb env {:?}", tb_env);
                println!("fb env {:?}", fb_env);
                env.variable_counter = fb_env.variable_counter;
                let (cmp, c_lh, c_rh) = resolve_cmp(cond, &mut env);
                let if_ex = RIf(
                    Box::new(RCmp(cmp, Box::new(c_lh), Box::new(c_rh))),
                    Box::new(tb_rco),
                    Box::new(fb_rco),
                );
                match is_tail {
                    true => if_ex,
                    false => lift(&if_ex, &mut env),
                }
            }
            RNot(ex) => {
                let ex_rco = ex.resolve_complex_(false, &mut env);
                lift(&RNot(Box::new(ex_rco)), &mut env)
            }
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
    let nv: Variable = format!("r{}", env.variable_counter);
    env.lifts.push((nv.clone(), expr.clone()));
    env.variable_counter += 1;
    RVar(nv)
}

fn resolve_cmp(c: &RExpr, mut env: &mut RCEnv) -> (CMP, RExpr, RExpr) {
    match c {
        RCmp(cmp, lh, rh) => (
            *cmp,
            lh.resolve_complex_(false, env),
            rh.resolve_complex_(false, env),
        ),
        RLet(v, ve, be) => {
            let ve_rco = ve.resolve_complex_(false, &mut env);
            env.renames.insert(v.clone(), ve_rco);
            resolve_cmp(be, env)
        }
        a => (
            CMP::EQ,
            RBool(true),
            lift(&a.resolve_complex_(false, env), env),
        ),
    }
}

fn resolve_expr(e: &RExpr, is_tail: bool, mut env: &mut RCEnv) -> RExpr {
    let expr = e.resolve_complex_(is_tail, &mut env);

    match env.lifts.len() {
        0 => expr,
        _ => recompose_lifts(&env.lifts),
    }
}

#[cfg(test)]
mod test_rco {
    use super::*;
    use crate::{
        common::{
            traits::InterpMut,
            types::{Answer, Answer::*},
        },
        rlang::Uniquify,
    };
    use pretty_assertions::assert_eq;

    type Test = (RProgram, RProgram, Answer);
    type Tests = Vec<Test>;

    #[test]
    fn test_rco() {
        let tests: Tests = vec![
            (RNum(5), RNum(5), S64(5)),
            (RRead, RLet!("r0", RRead, RVar!("r0")), S64(0)),
            (
                RNegate!(RNum(1)),
                RLet!("r0", RNegate!(RNum(1)), RVar!("r0")),
                S64(-1),
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
                S64(1),
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
                S64(11),
            ),
            (
                RAdd!(RAdd!(RNum(3), RNum(6)), RNum(2)),
                RLet!(
                    "r0",
                    RAdd!(RNum(3), RNum(6)),
                    RLet!("r1", RAdd!(RVar!("r0"), RNum(2)), RVar!("r1"))
                ),
                S64(11),
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
                S64(7),
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
                S64(5),
            ),
            (RBool(true), RBool(true), Bool(true)),
            (
                REQ!(RNum(5), RAdd!(RNum(5), RNum(4))),
                RLet!(
                    "r0",
                    RAdd!(RNum(5), RNum(4)),
                    RLet!("r1", REQ!(RNum(5), RVar!("r0")), RVar!("r1"))
                ),
                Bool(false),
            ),
            (
                REQ!(RNum(9), RAdd!(RNum(5), RNum(4))),
                RLet!(
                    "r0",
                    RAdd!(RNum(5), RNum(4)),
                    RLet!("r1", REQ!(RNum(9), RVar!("r0")), RVar!("r1"))
                ),
                Bool(true),
            ),
            (
                RAdd!(
                    RIf!(
                        RLT!(RRead, RNum(5)),
                        RNum(17),
                        RAdd!(RNum(8), RAdd!(RNum(9), RNum(10)))
                    ),
                    RAdd!(RRead, RNum(21))
                ),
                RLet!(
                    "r2",
                    RRead,
                    RLet!(
                        "r3",
                        RIf!(
                            RLT!(RVar!("r2"), RNum(5)),
                            RNum(17),
                            RLet!(
                                "r0",
                                RAdd!(RNum(9), RNum(10)),
                                RLet!("r1", RAdd!(RNum(8), RVar!("r0")), RVar!("r1"))
                            )
                        ),
                        RLet!(
                            "r4",
                            RRead,
                            RLet!(
                                "r5",
                                RAdd!(RVar!("r4"), RNum(21)),
                                RLet!("r6", RAdd!(RVar!("r3"), RVar!("r5")), RVar!("r6"))
                            )
                        )
                    )
                ),
                S64(39),
            ),
            (
                RIf!(REQ!(RNum(5), RNum(5)), RNum(42), RNum(24)),
                RIf!(REQ!(RNum(5), RNum(5)), RNum(42), RNum(24)),
                S64(42),
            ),
            (
                RIf!(REQ!(RNum(4), RNum(5)), RNum(42), RNum(24)),
                RIf!(REQ!(RNum(4), RNum(5)), RNum(42), RNum(24)),
                S64(24),
            ),
        ];

        for (start, expected_rco, expected_res) in tests {
            println!("Running RCO {:?}", start);
            let start_res = start.interp().unwrap();
            let uni = start.uniquify();
            let rco = uni.resolve_complex();
            let rco_res = rco.interp().unwrap();

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
