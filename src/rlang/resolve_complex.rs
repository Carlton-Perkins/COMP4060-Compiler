use std::collections::HashMap;

use super::Expr;
use super::Expr::*;
use crate::{common::types::Variable, rlang::RProgram};

type ProgramSeq = Vec<(Variable, Expr)>;

#[derive(Clone)]
pub struct RCEnv {
    lifts: ProgramSeq,
    renames: HashMap<Variable, Expr>,
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
            Num(_) => self.clone(),
            Read => lift(self, &mut env),
            Negate(ex) => {
                let ex_rco = ex.resolve_complex_(&mut env);
                lift(&Negate(Box::new(ex_rco)), &mut env)
            }
            Add(lh, rh) => {
                let lh_rco = lh.resolve_complex_(&mut env);
                let rh_rco = rh.resolve_complex_(&mut env);

                lift(&Add(Box::new(lh_rco), Box::new(rh_rco)), &mut env)
            }
            Let(v, ve, be) => {
                let ve_rco = ve.resolve_complex_(&mut env);
                env.renames.insert(v.clone(), ve_rco);
                let br_rco = be.resolve_complex_(&mut env);

                lift(&br_rco, &mut env)
            }
            Var(v) => env
                .renames
                .get(v)
                .expect(format!("RCO: Unbound var {:?}", v).as_str())
                .clone(),
        }
    }
}

fn recompose_lifts(seq: &ProgramSeq) -> Expr {
    let split = seq.split_first().unwrap();

    match split {
        (first, rest) if rest.len() > 0 => {
            let (label, expr) = first;
            Let(
                label.into(),
                Box::new(expr.clone()),
                Box::new(recompose_lifts(&rest.to_vec())),
            )
        }
        (first, _) => {
            let (label, expr) = first;
            Let(
                label.into(),
                Box::new(expr.clone()),
                Box::new(Var(label.into())),
            )
        }
    }
}

fn lift(expr: &Expr, env: &mut RCEnv) -> Expr {
    let nv: Variable = format!("r{}", env.lifts.len());
    env.lifts.push((nv.clone(), expr.clone()));
    Var(nv)
}

#[cfg(test)]
mod test_rco {
    use super::*;
    use crate::common::{traits::InterpMut, types::Number};
    use crate::rlang::uniquify::{UEnv, Uniquify};
    use crate::rlang::REnv;

    type Test = (RProgram, RProgram, Number);
    type Tests = Vec<Test>;

    #[test]
    fn test_rco() {
        let tests: Tests = vec![
            (Num(5), Num(5), 5),
            (Read, Let!("r0", Read, Var!("r0")), 0),
            (Negate!(Num(1)), Let!("r0", Negate!(Num(1)), Var!("r0")), -1),
            (
                Add!(Read, Read),
                Let!(
                    "r0",
                    Read,
                    Let!(
                        "r1",
                        Read,
                        Let!("r2", Add!(Var!("r0"), Var!("r1")), Var!("r2"))
                    )
                ),
                1,
            ),
            (
                Add(
                    Box::new(Add(Box::new(Num(3)), Box::new(Num(6)))),
                    Box::new(Num(2)),
                ),
                Let(
                    "r0".into(),
                    Box::new(Add(Box::new(Num(3)), Box::new(Num(6)))),
                    Box::new(Let(
                        "r1".into(),
                        Box::new(Add(Box::new(Var("r0".into())), Box::new(Num(2)))),
                        Box::new(Var("r1".into())),
                    )),
                ),
                11,
            ),
            (
                Add!(Add!(Num(3), Num(6)), Num(2)),
                Let!(
                    "r0",
                    Add!(Num(3), Num(6)),
                    Let!("r1", Add!(Var!("r0"), Num(2)), Var!("r1"))
                ),
                11,
            ),
            (
                Add!(Add!(Num(3), Num(6)), Negate!(Add!(Read, Num(2)))),
                Let!(
                    "r0",
                    Add!(Num(3), Num(6)),
                    Let!(
                        "r1",
                        Read,
                        Let!(
                            "r2",
                            Add!(Var!("r1"), Num(2)),
                            Let!(
                                "r3",
                                Negate!(Var!("r2")),
                                Let!("r4", Add!(Var!("r0"), Var!("r3")), Var!("r4"))
                            )
                        )
                    )
                ),
                7,
            ),
            (
                Add!(
                    Add!(Num(2), Num(3)),
                    Let!("x", Read, Add!(Var!("x"), Var!("x")))
                ),
                Let!(
                    "r0",
                    Add!(Num(2), Num(3)),
                    Let!(
                        "r1",
                        Read,
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

        for (start, expected_rco, expected_res) in tests {
            println!("Running RCO {:?}", start);
            let start_res = start.interp(&mut REnv::new());
            let uni = start.uniquify(&mut UEnv::new());
            let rco = uni.resolve_complex();
            let rco_res = rco.interp(&mut REnv::new());

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
                ("a".to_string(), Num(4)),
                ("b".to_string(), Num(4)),
                ("c".to_string(), Add!(Var!("a"), Var!("b"))),
            ],
            Let!(
                "a",
                Num(4),
                Let!(
                    "b",
                    Num(4),
                    Let!("c", Add!(Var!("a"), Var!("b")), Var!("c"))
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
