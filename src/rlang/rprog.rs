use crate::common::types::{Number, Variable};
pub use crate::common::{InterpMut, IsPure};
use std::collections::HashMap;

pub type RProgram = Expr;
use Expr::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(i64),
    Read,
    Negate(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Let(Variable, Box<Expr>, Box<Expr>),
    Var(Variable),
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

impl IsPure for Expr {
    fn is_pure(&self) -> bool {
        match self {
            Num(_) => true,
            Read => false,
            Negate(ex) => ex.is_pure(),
            Add(lh, rh) => lh.is_pure() && rh.is_pure(),
            Let(_, ve, be) => ve.is_pure() && be.is_pure(),
            Var(_) => true,
        }
    }
}

impl InterpMut for Expr {
    type Env = REnv;
    type Output = Number;

    fn interp(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            Num(n) => *n,
            Read => {
                let res = env.read_count as i64;
                env.read_count += 1;
                res
            }
            Negate(ex) => -1 * ex.interp(env),
            Add(lh, rh) => lh.interp(env) + rh.interp(env),
            Let(v, ve, be) => {
                let value = ve.interp(env);
                env.vars.insert(v.clone(), value);
                be.interp(env)
            }
            Var(n) => env.vars[n],
        }
    }
}

#[cfg(test)]
mod test_rprog {
    use super::*;

    use crate::rlang::rmacros::*;

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
            (Num(5), 5),
            (Num(-5), -5),
            (Add(Box::new(Num(5)), Box::new(Num(6))), 11),
            (Add!(Num(5), Num(6)), 11),
            (Add(Box::new(Read), Box::new(Read)), 1),
            (Add!(Read, Read), 1),
            (Read, 0),
            (Negate(Box::new(Num(5))), -5),
            (Negate!(Num(5)), -5),
            (
                Add(Box::new(Num(5)), Box::new(Negate(Box::new(Num(6))))),
                -1,
            ),
            (Add!(Num(5), Negate!(Num(6))), -1),
            (Add(Box::new(Read), Box::new(Negate(Box::new(Num(6))))), -6),
            (Add!(Read, Negate!(Num(6))), -6),
            (
                Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(6))))))),
                -6,
            ),
            (Negate!(Negate!(Negate!(Num(6)))), -6),
            (
                Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(0))))))),
                0,
            ),
            (Negate!(Negate!(Negate!(Num(0)))), 0),
        ];

        a_interp_all(tests);
    }

    #[test]
    fn test_r1() {
        let tests = vec![
            (Let("0".into(), Box::new(Num(0)), Box::new(Read)), 0),
            (Let!("0", Num(0), Read), 0),
            (
                Let(
                    "0".into(),
                    Box::new(Num(0)),
                    Box::new(Let("0".into(), Box::new(Num(1)), Box::new(Var("0".into())))),
                ),
                1,
            ),
            (Let!("0", Num(0), Let!("0", Num(1), Var!("0"))), 1),
            (
                Let(
                    "0".into(),
                    Box::new(Num(4)),
                    Box::new(Let(
                        "1".into(),
                        Box::new(Num(5)),
                        Box::new(Add(Box::new(Var("0".into())), Box::new(Var("1".into())))),
                    )),
                ),
                9,
            ),
            (
                Let!("0", Num(4), Let!("1", Num(5), Add!(Var!("0"), Var!("1")))),
                9,
            ),
            (
                Let(
                    "0".into(),
                    Box::new(Read),
                    Box::new(Let(
                        "1".into(),
                        Box::new(Read),
                        Box::new(Add(Box::new(Var("0".into())), Box::new(Var("1".into())))),
                    )),
                ),
                1,
            ),
            (
                Let!("0", Read, Let!("1", Read, Add!(Var!("0"), Var!("1")))),
                1,
            ),
        ];

        a_interp_all(tests);
    }

    fn two_n(n: usize) -> Expr {
        match n {
            0 => Num(1),
            n => Add(Box::new(two_n(n - 1)), Box::new(two_n(n - 1))),
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
