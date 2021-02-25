use std::collections::HashMap;

use crate::common::types::Variable;
use crate::rlang::rprog::{RExpr, RProgram};

pub trait Uniquify {
    type Env;
    type Output;
    fn uniquify(&self, env: &mut Self::Env) -> Self::Output;
}

#[derive(Clone)]
pub struct UEnv {
    var_counter: usize,
    var_map: HashMap<Variable, Variable>,
}

impl UEnv {
    pub fn new() -> Self {
        UEnv {
            var_counter: 0,
            var_map: HashMap::new(),
        }
    }
}

impl Uniquify for RProgram {
    type Env = UEnv;
    type Output = RProgram;

    fn uniquify(&self, env: &mut Self::Env) -> Self::Output {
        use RExpr::*;
        match self {
            RNum(_) => self.clone(),
            RRead => self.clone(),
            RNegate(ex) => RNegate(Box::new(ex.uniquify(env))),
            RAdd(lh, rh) => RAdd(Box::new(lh.uniquify(env)), Box::new(rh.uniquify(env))),
            RLet(v, ve, be) => {
                let nv = format!("u{}", env.var_counter);
                env.var_counter += 1;
                let nve = ve.uniquify(env);

                env.var_map.insert(v.into(), nv.clone());
                let nbe = be.uniquify(env);

                RLet(nv, Box::new(nve), Box::new(nbe))
            }
            RVar(v) => match env.var_map.get(v) {
                Some(nv) => RVar(nv.into()),
                None => panic!("Uniquify unbound variable"),
            },
        }
    }
}

#[cfg(test)]
mod test_uniquify {
    use super::*;
    use crate::common::traits::InterpMut;
    use crate::rlang::REnv;
    use crate::rlang::{randp, RandEnv};
    use crate::{common::types::Number, rlang::RExpr::*};

    type Test = (RProgram, RProgram, Number);
    type Tests = Vec<Test>;

    fn a_uni((start_program, expected_uni_program, expected_res): Test) {
        let uni_program = start_program.uniquify(&mut UEnv::new());
        let uni_res = uni_program.interp(&mut REnv::new());
        let start_res = start_program.interp(&mut REnv::new());

        assert_eq!(
            start_res, expected_res,
            "Init program evaluates incorrectly, {:?} -> {} != {}",
            start_program, start_res, expected_res
        );
        assert_eq!(
            uni_res, expected_res,
            "Uniquified program evaluates incorrectly, {:?} -> {:?}, {} != {}",
            start_program, uni_program, uni_res, expected_res
        );
        assert_eq!(
            uni_program, expected_uni_program,
            "Uniquified program varies from expected, {:?} -> {:?} / {:?}",
            start_program, uni_program, expected_uni_program
        );
    }

    fn a_uni_all(tests: Tests) {
        for test in tests {
            a_uni(test);
        }
    }

    #[test]
    fn test_uni() {
        let tests = vec![
            (RNum(5), RNum(5), 5),
            (
                RLet("0".into(), Box::new(RNum(5)), Box::new(RVar("0".into()))),
                RLet("u0".into(), Box::new(RNum(5)), Box::new(RVar("u0".into()))),
                5,
            ),
            (
                RLet(
                    "0".into(),
                    Box::new(RNum(5)),
                    Box::new(RLet(
                        "1".into(),
                        Box::new(RRead),
                        Box::new(RAdd(Box::new(RVar("0".into())), Box::new(RVar("1".into())))),
                    )),
                ),
                RLet(
                    "u0".into(),
                    Box::new(RNum(5)),
                    Box::new(RLet(
                        "u1".into(),
                        Box::new(RRead),
                        Box::new(RAdd(
                            Box::new(RVar("u0".into())),
                            Box::new(RVar("u1".into())),
                        )),
                    )),
                ),
                5,
            ),
            (
                RLet(
                    "0".into(),
                    Box::new(RNum(5)),
                    Box::new(RLet(
                        "0".into(),
                        Box::new(RAdd(Box::new(RRead), Box::new(RNum(1)))),
                        Box::new(RAdd(Box::new(RVar("0".into())), Box::new(RVar("0".into())))),
                    )),
                ),
                RLet(
                    "u0".into(),
                    Box::new(RNum(5)),
                    Box::new(RLet(
                        "u1".into(),
                        Box::new(RAdd(Box::new(RRead), Box::new(RNum(1)))),
                        Box::new(RAdd(
                            Box::new(RVar("u1".into())),
                            Box::new(RVar("u1".into())),
                        )),
                    )),
                ),
                2,
            ),
        ];

        a_uni_all(tests);
    }

    #[test]
    fn test_uniquify_randp() {
        for depth in 0..10 {
            for _ in 0..100 {
                let program = randp(depth, &RandEnv::new());
                let program_res = program.interp(&mut REnv::new());
                let uni = program.uniquify(&mut UEnv::new());
                let unit_res = uni.interp(&mut REnv::new());

                assert_eq!(program_res, unit_res)
            }
        }
    }
}
