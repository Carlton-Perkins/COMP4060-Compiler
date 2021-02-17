use crate::rlang::Expr::*;
use crate::rlang::{Expr, IsPure, Variable};
use std::collections::HashMap;

#[derive(Clone)]

pub struct OptEnv {
    vars: HashMap<Variable, Expr>,
}

impl OptEnv {
    pub fn new() -> Self {
        OptEnv {
            vars: HashMap::new(),
        }
    }
}

pub fn opt(e: Expr, env: &OptEnv) -> Expr {
    match e {
        Num(_) => e,
        Read => e,
        Negate(ex) => {
            let o = opt(*ex, env);
            match o {
                Num(n) => Num(-1 * n),
                Read => Negate(Box::new(o)),
                Negate(n) => *n,
                Add(_, _) => Negate(Box::new(o)),
                n => Negate(Box::new(n)),
            }
        }
        Add(le, re) => {
            let o = (opt(*le, env), opt(*re, env));

            match o.clone() {
                (Num(l), Num(r)) => Num(l + r),
                (Num(l), Add(r1, r2)) => match *r1 {
                    Num(r) => Add(Box::new(Num(l + r)), r2),
                    _ => Add(Box::new(o.0), Box::new(o.1)),
                },
                (Add(l1, l2), Num(r)) => match *l1 {
                    Num(l) => Add(Box::new(Num(l + r)), l2),
                    _ => Add(Box::new(o.0), Box::new(o.1)),
                },
                (Add(l1, l2), Add(r1, r2)) => match (*l1, *r1) {
                    (Num(l), Num(r)) => Add(Box::new(Num(l + r)), Box::new(Add(l2, r2))),
                    _ => Add(Box::new(o.0), Box::new(o.1)),
                },
                (l, Num(n)) => Add(Box::new(Num(n)), Box::new(l)),
                _ => Add(Box::new(o.0), Box::new(o.1)),
            }
        }
        Let(id, ve, be) => {
            let o_ve = opt(*ve, env);

            if o_ve.is_pure() {
                let mut new_env = env.clone();
                new_env.vars.insert(id, o_ve);
                opt(*be, &new_env)
            } else {
                let o_be = opt(*be, env);
                Let(id, Box::new(o_ve), Box::new(o_be))
            }
        }
        Var(id) => match env.vars.get(&id) {
            Some(e) => e.clone(),
            None => Var(id),
        },
    }
}

#[cfg(test)]
mod test_ropt {
    use super::*;
    use crate::rlang::rrandp::{randp, RandEnv};
    use crate::rlang::{Env, InterpMut};

    fn a_opt(e: Expr, expected_opt: Expr, expected_result: i64) {
        println!("{:?}", e);
        let e_res = e.clone().interp(&mut Env::new());
        let opt = opt(e.clone(), &OptEnv::new());
        let opt_res = opt.clone().interp(&mut Env::new());

        assert_eq!(
            opt, expected_opt,
            "Optimization for {:?} does not result in the expected optimization {:?}",
            opt, expected_opt
        );
        assert_eq!(
            e_res, expected_result,
            "Evaluation for {:?} does not evaluate to {}",
            e, expected_result
        );
        assert_eq!(e_res, opt_res, "Optimization for {:?} -> {} does not evaluate the same as original expression {:?} -> {} ", opt, opt_res, e, e_res);
    }

    fn a_opt_all(vec: Vec<(Expr, Expr, i64)>) {
        for (e, expect_opt, expect_res) in vec {
            a_opt(e, expect_opt, expect_res)
        }
    }

    #[test]
    fn test_opt_r0() {
        let test_expr = vec![
            (Num(5), Num(5), 5),
            (Negate(Box::new(Num(5))), Num(-5), -5),
            (Negate(Box::new(Read)), Negate(Box::new(Read)), 0),
            (Add(Box::new(Num(3)), Box::new(Num(2))), Num(5), 5),
            (
                Add(Box::new(Num(3)), Box::new(Read)),
                Add(Box::new(Num(3)), Box::new(Read)),
                3,
            ),
        ];

        a_opt_all(test_expr);
    }

    #[test]
    fn test_opt_r1() {
        let test_expr = vec![
            (
                Let("0".into(), Box::new(Num(0)), Box::new(Num(1))),
                Num(1),
                1,
            ),
            (Let("0".into(), Box::new(Num(5)), Box::new(Read)), Read, 0),
            (
                Let("0".into(), Box::new(Num(5)), Box::new(Var("0".into()))),
                Num(5),
                5,
            ),
            (
                Let("0".into(), Box::new(Read), Box::new(Var("0".into()))),
                Let("0".into(), Box::new(Read), Box::new(Var("0".into()))),
                0,
            ),
            (
                Let(
                    "0".into(),
                    Box::new(Num(5)),
                    Box::new(Let("0".into(), Box::new(Num(6)), Box::new(Var("0".into())))),
                ),
                Num(6),
                6,
            ),
            (
                Let(
                    "0".into(),
                    Box::new(Num(3)),
                    Box::new(Let(
                        "1".into(),
                        Box::new(Num(2)),
                        Box::new(Let(
                            "2".into(),
                            Box::new(Add(Box::new(Var("0".into())), Box::new(Var("1".into())))),
                            Box::new(Add(Box::new(Read), Box::new(Var("2".into())))),
                        )),
                    )),
                ),
                Add(Box::new(Num(5)), Box::new(Read)),
                5,
            ),
        ];

        a_opt_all(test_expr);
    }

    #[test]
    #[ignore = "Slow"]
    fn test_randp_opt() {
        for depth in 0..20 {
            for _ in 0..100 {
                let e = randp(depth, &RandEnv::new());
                let e_res = e.clone().interp(&mut Env::new());
                let opt = opt(e.clone(), &OptEnv::new());
                let opt_res = opt.interp(&mut Env::new());

                assert_eq!(e_res, opt_res, "'{:?}' does not equal '{:?}'", e, opt);
            }
        }
    }
}
