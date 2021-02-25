pub use crate::common::traits::{InterpMut, IsPure};
use crate::common::types::{Number, Variable};
use std::collections::HashMap;

pub type RProgram = RExpr;
use RExpr::*;

#[derive(Debug, PartialEq, Clone)]
pub enum RExpr {
    RNum(i64),
    RRead,
    RNegate(Box<RExpr>),
    RAdd(Box<RExpr>, Box<RExpr>),
    RLet(Variable, Box<RExpr>, Box<RExpr>),
    RVar(Variable),
}

pub struct REnv {
    read_count: isize,
    vars: HashMap<Variable, Number>,
}

impl REnv {
    pub fn new() -> Self {
        REnv {
            read_count: 0,
            vars: HashMap::new(),
        }
    }
}

impl IsPure for RExpr {
    fn is_pure(&self) -> bool {
        match self {
            RNum(_) => true,
            RRead => false,
            RNegate(ex) => ex.is_pure(),
            RAdd(lh, rh) => lh.is_pure() && rh.is_pure(),
            RLet(_, ve, be) => ve.is_pure() && be.is_pure(),
            RVar(_) => true,
        }
    }
}

impl InterpMut for RExpr {
    type Env = REnv;
    type Output = Number;

    fn interp(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            RNum(n) => *n,
            RRead => {
                let res = env.read_count as i64;
                env.read_count += 1;
                res
            }
            RNegate(ex) => -1 * ex.interp(env),
            RAdd(lh, rh) => lh.interp(env) + rh.interp(env),
            RLet(v, ve, be) => {
                let value = ve.interp(env);
                env.vars.insert(v.clone(), value);
                be.interp(env)
            }
            RVar(n) => *env
                .vars
                .get(n)
                .expect(format!("RInterp: Unbound variable {:?}", n).as_str()),
        }
    }
}

#[cfg(test)]
mod test_rprog {
    use super::*;

    fn a_interp(expr: RProgram, expect: Number) {
        let res = expr.interp(&mut REnv::new());
        assert_eq!(
            res, expect,
            "Program {:?} does not eval to {}, but instead {}",
            expr, expect, res
        );
    }

    fn a_interp_all(vec: Vec<(RProgram, Number)>) {
        for (e, ex) in vec {
            a_interp(e, ex)
        }
    }

    #[test]
    fn test_r0() {
        let tests = vec![
            (RNum(5), 5),
            (RNum(-5), -5),
            (RAdd(Box::new(RNum(5)), Box::new(RNum(6))), 11),
            (RAdd!(RNum(5), RNum(6)), 11),
            (RAdd(Box::new(RRead), Box::new(RRead)), 1),
            (RAdd!(RRead, RRead), 1),
            (RRead, 0),
            (RNegate(Box::new(RNum(5))), -5),
            (RNegate!(RNum(5)), -5),
            (
                RAdd(Box::new(RNum(5)), Box::new(RNegate(Box::new(RNum(6))))),
                -1,
            ),
            (RAdd!(RNum(5), RNegate!(RNum(6))), -1),
            (
                RAdd(Box::new(RRead), Box::new(RNegate(Box::new(RNum(6))))),
                -6,
            ),
            (RAdd!(RRead, RNegate!(RNum(6))), -6),
            (
                RNegate(Box::new(RNegate(Box::new(RNegate(Box::new(RNum(6))))))),
                -6,
            ),
            (RNegate!(RNegate!(RNegate!(RNum(6)))), -6),
            (
                RNegate(Box::new(RNegate(Box::new(RNegate(Box::new(RNum(0))))))),
                0,
            ),
            (RNegate!(RNegate!(RNegate!(RNum(0)))), 0),
        ];

        a_interp_all(tests);
    }

    #[test]
    fn test_r1() {
        let tests = vec![
            (RLet("0".into(), Box::new(RNum(0)), Box::new(RRead)), 0),
            (RLet!("0", RNum(0), RRead), 0),
            (
                RLet(
                    "0".into(),
                    Box::new(RNum(0)),
                    Box::new(RLet(
                        "0".into(),
                        Box::new(RNum(1)),
                        Box::new(RVar("0".into())),
                    )),
                ),
                1,
            ),
            (RLet!("0", RNum(0), RLet!("0", RNum(1), RVar!("0"))), 1),
            (
                RLet(
                    "0".into(),
                    Box::new(RNum(4)),
                    Box::new(RLet(
                        "1".into(),
                        Box::new(RNum(5)),
                        Box::new(RAdd(Box::new(RVar("0".into())), Box::new(RVar("1".into())))),
                    )),
                ),
                9,
            ),
            (
                RLet!(
                    "0",
                    RNum(4),
                    RLet!("1", RNum(5), RAdd!(RVar!("0"), RVar!("1")))
                ),
                9,
            ),
            (
                RLet(
                    "0".into(),
                    Box::new(RRead),
                    Box::new(RLet(
                        "1".into(),
                        Box::new(RRead),
                        Box::new(RAdd(Box::new(RVar("0".into())), Box::new(RVar("1".into())))),
                    )),
                ),
                1,
            ),
            (
                RLet!("0", RRead, RLet!("1", RRead, RAdd!(RVar!("0"), RVar!("1")))),
                1,
            ),
        ];

        a_interp_all(tests);
    }

    fn two_n(n: usize) -> RExpr {
        match n {
            0 => RNum(1),
            n => RAdd(Box::new(two_n(n - 1)), Box::new(two_n(n - 1))),
        }
    }

    #[test]
    fn test_two_n() {
        a_interp(two_n(0), 1);
        a_interp(two_n(1), 2);
        a_interp(two_n(2), 4);
        a_interp(two_n(3), 8);
        a_interp(two_n(4), 16);
        a_interp(two_n(5), 32);
    }
}
