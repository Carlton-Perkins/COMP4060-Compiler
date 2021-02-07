use std::collections::HashMap;
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(i64),
    Read,
    Negate(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Let(Var, Box<Expr>, Box<Expr>),
    Var(Var),
}

pub type Var = usize;

use i64 as OType;
use Expr::*;

pub type Program<'a> = (Expr, &'a mut Env);
pub struct Env {
    read_count: isize,
    vars: HashMap<Var, OType>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            read_count: 0,
            vars: HashMap::new(),
        }
    }
}

impl Expr {
    pub fn is_pure(e: &Self) -> bool {
        match e {
            Num(_) => true,
            Read => false,
            Negate(ex) => Expr::is_pure(ex),
            Add(lh, rh) => Expr::is_pure(lh) && Expr::is_pure(rh),
            Let(_, ve, be) => Expr::is_pure(ve) && Expr::is_pure(be),
            Var(_) => true,
        }
    }
    
}

pub fn interp((expr, mut env): Program) -> OType {
    match expr {
        Num(n) => n,
        Read => {
            let res = env.read_count as i64;
            env.read_count += 1;
            res
        }
        Negate(ex) => -1 * interp((*ex, &mut env)),
        Add(lh, rh) => interp((*lh, &mut env)) + interp((*rh, &mut env)),
        Let(v, ve, be) => {
            let value = interp((*ve, env));
            env.vars.insert(v, value);
            interp((*be, env))
        }
        Var(n) => env.vars[&n],
    }
}

#[cfg(test)]
mod test_rprog {
    use super::*;

    fn a_interp(expr: Expr, expect: OType) {
        let prog = (expr.clone(), &mut Env::new());
        let res = interp(prog);
        assert_eq!(
            res, expect,
            "Program {:?} does not eval to {}, but instead {}",
            expr, expect, res
        );
    }

    fn a_interp_all(vec: Vec<(Expr, OType)>) {
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
            (Add(Box::new(Read), Box::new(Read)), 1),
            (Read, 0),
            (Negate(Box::new(Num(5))), -5),
            (
                Add(Box::new(Num(5)), Box::new(Negate(Box::new(Num(6))))),
                -1,
            ),
            (Add(Box::new(Read), Box::new(Negate(Box::new(Num(6))))), -6),
            (
                Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(6))))))),
                -6,
            ),
            (
                Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(0))))))),
                0,
            ),
        ];

        a_interp_all(tests);
    }

    #[test]
    fn test_r1() {
        let tests = vec![
            (Let(0, Box::new(Num(0)), Box::new(Read)), 0),
            (
                Let(
                    0,
                    Box::new(Num(0)),
                    Box::new(Let(0, Box::new(Num(1)), Box::new(Var(0)))),
                ),
                1,
            ),
            (
                Let(
                    0,
                    Box::new(Num(4)),
                    Box::new(Let(
                        1,
                        Box::new(Num(5)),
                        Box::new(Add(Box::new(Var(0)), Box::new(Var(1)))),
                    )),
                ),
                9,
            ),
            (
                Let(
                    0,
                    Box::new(Read),
                    Box::new(Let(
                        1,
                        Box::new(Read),
                        Box::new(Add(Box::new(Var(0)), Box::new(Var(1)))),
                    )),
                ),
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