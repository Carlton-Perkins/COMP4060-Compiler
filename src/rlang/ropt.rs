pub use crate::common::traits::Opt;
use crate::{
    common::{traits::IsPure, types::Variable},
    rlang::{RExpr, RExpr::*, CMP::*},
};
use std::collections::HashMap;

#[derive(Clone)]

pub struct OptEnv {
    vars: HashMap<Variable, RExpr>,
}

impl OptEnv {
    pub fn new() -> Self {
        OptEnv {
            vars: HashMap::new(),
        }
    }
}

impl Opt for RExpr {
    type Env = OptEnv;

    fn opt(&self) -> Self {
        self.opt_(&OptEnv::new())
    }

    fn opt_(&self, env: &Self::Env) -> Self {
        match self {
            RNum(_) => self.clone(),
            RRead => self.clone(),
            RNegate(ex) => {
                let o = ex.opt_(env);
                match o {
                    RNum(n) => RNum(-1 * n),
                    RRead => RNegate(Box::new(o)),
                    RNegate(n) => *n,
                    RAdd(_, _) => RNegate(Box::new(o)),
                    n => RNegate(Box::new(n)),
                }
            }
            RAdd(le, re) => {
                let o = (le.opt_(env), re.opt_(env));
                match o.clone() {
                    (RNum(l), RNum(r)) => RNum(l + r),
                    (RNum(l), RAdd(r1, r2)) => match *r1 {
                        RNum(r) => RAdd(Box::new(RNum(l + r)), r2),
                        _ => RAdd(Box::new(o.0), Box::new(o.1)),
                    },
                    (RAdd(l1, l2), RNum(r)) => match *l1 {
                        RNum(l) => RAdd(Box::new(RNum(l + r)), l2),
                        _ => RAdd(Box::new(o.0), Box::new(o.1)),
                    },
                    (RAdd(l1, l2), RAdd(r1, r2)) => match (*l1, *r1) {
                        (RNum(l), RNum(r)) => RAdd(Box::new(RNum(l + r)), Box::new(RAdd(l2, r2))),
                        _ => RAdd(Box::new(o.0), Box::new(o.1)),
                    },
                    (l, RNum(n)) => RAdd(Box::new(RNum(n)), Box::new(l)),
                    _ => RAdd(Box::new(o.0), Box::new(o.1)),
                }
            }
            RLet(id, ve, be) => {
                let o_ve = ve.opt_(env);
                if o_ve.is_pure() {
                    let mut new_env = env.clone();
                    new_env.vars.insert(id.clone(), o_ve);
                    be.opt_(&new_env)
                } else {
                    let o_be = be.opt_(env);
                    RLet(id.clone(), Box::new(o_ve), Box::new(o_be))
                }
            }
            RVar(id) => match env.vars.get(id) {
                Some(e) => e.clone(),
                None => RVar(id.clone()),
            },
            RBool(_) => self.clone(),
            RCmp(cmp, lh, rh) => {
                let o = (lh.opt_(env), rh.opt_(env));
                match o {
                    (RNum(a), RNum(b)) => RBool(match cmp {
                        EQ => a == b,
                        LT => a < b,
                        LEQ => a <= b,
                        GEQ => a >= b,
                        GT => a > b,
                    }),
                    _ => RCmp(*cmp, Box::new(o.0), Box::new(o.1)),
                }
            }
            RIf(c, t, f) => {
                let o = (c.opt_(env), t.opt_(env), f.opt_(env));
                match o {
                    (RBool(cond), a, b) => match cond {
                        true => a,
                        false => b,
                    },
                    (RNot(ncond), a, b) => RIf(Box::new(*ncond), Box::new(b), Box::new(a)),
                    _ => RIf(Box::new(o.0), Box::new(o.1), Box::new(o.2)),
                }
            }
            RNot(ex) => {
                let o_ex = ex.opt_(env);
                match o_ex {
                    RBool(b) => RBool(!b),
                    RNot(b) => *b,
                    _ => RNot(Box::new(o_ex)),
                }
            }
        }
    }
}

#[cfg(test)]
mod test_ropt {
    use super::*;
    use crate::{
        common::{traits::InterpMut, types::Answer, types::Answer::*},
        rlang::randp,
    };
    use pretty_assertions::assert_eq;

    fn a_opt(e: RExpr, expected_opt: RExpr, expected_result: Answer) {
        println!("{:?}", e);
        let e_res = e.interp().unwrap();
        let opt = e.opt();
        let opt_res = opt.interp().unwrap();

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

    fn a_opt_all(vec: Vec<(RExpr, RExpr, Answer)>) {
        for (e, expect_opt, expect_res) in vec {
            a_opt(e, expect_opt, expect_res)
        }
    }

    #[test]
    fn test_opt_r0() {
        let test_expr = vec![
            (RNum(5), RNum(5), S64(5)),
            (RNegate(Box::new(RNum(5))), RNum(-5), S64(-5)),
            (RNegate(Box::new(RRead)), RNegate(Box::new(RRead)), S64(0)),
            (RAdd(Box::new(RNum(3)), Box::new(RNum(2))), RNum(5), S64(5)),
            (
                RAdd(Box::new(RNum(3)), Box::new(RRead)),
                RAdd(Box::new(RNum(3)), Box::new(RRead)),
                S64(3),
            ),
        ];

        a_opt_all(test_expr);
    }

    #[test]
    fn test_opt_r1() {
        let test_expr = vec![
            (
                RLet("0".into(), Box::new(RNum(0)), Box::new(RNum(1))),
                RNum(1),
                S64(1),
            ),
            (
                RLet("0".into(), Box::new(RNum(5)), Box::new(RRead)),
                RRead,
                S64(0),
            ),
            (
                RLet("0".into(), Box::new(RNum(5)), Box::new(RVar("0".into()))),
                RNum(5),
                S64(5),
            ),
            (
                RLet("0".into(), Box::new(RRead), Box::new(RVar("0".into()))),
                RLet("0".into(), Box::new(RRead), Box::new(RVar("0".into()))),
                S64(0),
            ),
            (
                RLet(
                    "0".into(),
                    Box::new(RNum(5)),
                    Box::new(RLet(
                        "0".into(),
                        Box::new(RNum(6)),
                        Box::new(RVar("0".into())),
                    )),
                ),
                RNum(6),
                S64(6),
            ),
            (
                RLet(
                    "0".into(),
                    Box::new(RNum(3)),
                    Box::new(RLet(
                        "1".into(),
                        Box::new(RNum(2)),
                        Box::new(RLet(
                            "2".into(),
                            Box::new(RAdd(Box::new(RVar("0".into())), Box::new(RVar("1".into())))),
                            Box::new(RAdd(Box::new(RRead), Box::new(RVar("2".into())))),
                        )),
                    )),
                ),
                RAdd(Box::new(RNum(5)), Box::new(RRead)),
                S64(5),
            ),
        ];

        a_opt_all(test_expr);
    }

    #[test]
    fn test_opt_r2() {
        let test_expr = vec![
            (RBool(true), RBool(true), Bool(true)),
            (RNot!(RBool(true)), RBool(false), Bool(false)),
            (REQ!(RNum(5), RNum(5)), RBool(true), Bool(true)),
            (RLEQ!(RNum(5), RNum(5)), RBool(true), Bool(true)),
            (RLEQ!(RNum(5), RRead), RLEQ!(RNum(5), RRead), Bool(false)),
            (RIf!(RBool(true), RNum(5), RNum(-8)), RNum(5), S64(5)),
            (
                RNot!(RIf!(RBool(true), RBool(true), RBool(false))),
                RBool(false),
                Bool(false),
            ),
            (
                RNot!(RIf!(RBool(false), RBool(true), REQ!(RRead, RRead))),
                RNot!(REQ!(RRead, RRead)),
                Bool(true),
            ),
            (
                RIf!(RNot!(RBool(true)), RBool(true), REQ!(RRead, RRead)),
                REQ!(RRead, RRead),
                Bool(false),
            ),
            (
                RIf!(RNot!(REQ!(RRead, RRead)), RNum(4), RNum(77)),
                RIf!(REQ!(RRead, RRead), RNum(77), RNum(4)),
                S64(4),
            ),
            (
                RIf!(REQ!(RNum(5), RNum(5)), RNum(5), RNum(-8)),
                RNum(5),
                S64(5),
            ),
            (
                RIf!(REQ!(RNum(5), RRead), RNum(5), RNum(-8)),
                RIf!(REQ!(RNum(5), RRead), RNum(5), RNum(-8)),
                S64(-8),
            ),
        ];

        a_opt_all(test_expr);
    }

    #[test]
    fn test_randp_opt() {
        for depth in 0..10 {
            for _ in 0..100 {
                let e = randp(depth);
                let e_res = e.interp();
                let opt = e.opt();
                let opt_res = opt.interp();

                assert_eq!(e_res, opt_res, "'{:?}' does not equal '{:?}'", e, opt);
            }
        }
    }
}
