use std::collections::HashMap;

use crate::rlang::rprog::{Expr, Program, Variable};

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
    fn new() -> Self {
        UEnv {
            var_counter: 0,
            var_map: HashMap::new(),
        }
    }
}

impl Uniquify for Program {
    type Env = UEnv;
    type Output = Program;

    fn uniquify(&self, env: &mut Self::Env) -> Self::Output {
        unimplemented!()
    }
}

#[cfg(test)]
mod test_uniquify {
    use super::*;
    use crate::rlang::Expr::*;
    use crate::rlang::{Env, InterpMut, OType};

    type Test = (Program, Program, OType);
    type Tests = Vec<Test>;

    fn a_uni((start_program, expected_uni_program, expected_res): Test) {
        let uni_program = start_program.uniquify(&mut UEnv::new());
        let uni_res = uni_program.interp(&mut Env::new());
        let start_res = start_program.interp(&mut Env::new());

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
            (Num(5), Num(5), 5),
            (
                Let("0".into(), Box::new(Num(5)), Box::new(Var("0".into()))),
                Let("u0".into(), Box::new(Num(5)), Box::new(Var("u0".into()))),
                5,
            ),
            (
                Let(
                    "0".into(),
                    Box::new(Num(5)),
                    Box::new(Let(
                        "1".into(),
                        Box::new(Read),
                        Box::new(Add(Box::new(Var("0".into())), Box::new(Var("1".into())))),
                    )),
                ),
                Let(
                    "u0".into(),
                    Box::new(Num(5)),
                    Box::new(Let(
                        "u1".into(),
                        Box::new(Read),
                        Box::new(Add(Box::new(Var("u0".into())), Box::new(Var("u1".into())))),
                    )),
                ),
                5,
            ),
            (
                Let(
                    "0".into(),
                    Box::new(Num(5)),
                    Box::new(Let(
                        "0".into(),
                        Box::new(Read),
                        Box::new(Add(Box::new(Var("0".into())), Box::new(Var("0".into())))),
                    )),
                ),
                Let(
                    "u0".into(),
                    Box::new(Num(5)),
                    Box::new(Let(
                        "u1".into(),
                        Box::new(Read),
                        Box::new(Add(Box::new(Var("u0".into())), Box::new(Var("u1".into())))),
                    )),
                ),
                5,
            ),
        ];

        a_uni_all(tests);
    }
}
