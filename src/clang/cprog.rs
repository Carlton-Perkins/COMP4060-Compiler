use crate::common::{
    traits::InterpMut,
    types::{Label, Number, Variable},
};
use std::collections::HashMap;

type CLabelMapping = HashMap<Label, CTail>;
pub type CProgram = CLabelMapping;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CArgument {
    Num(Number),
    Var(Variable),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CExpression {
    Arg(CArgument),
    Read,
    Negate(CArgument),
    Add(CArgument, CArgument),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CStatement {
    Set(Variable, CExpression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CTail {
    Return(CArgument),
    Seq(CStatement, Box<CTail>),
}

#[derive(Clone)]
pub struct CEnv {
    read_count: usize,
    block_map: CLabelMapping,
    var_map: HashMap<Variable, Number>,
}

impl CEnv {
    pub fn new(prog: &CProgram) -> Self {
        CEnv {
            read_count: 0,
            block_map: prog.clone(),
            var_map: HashMap::new(),
        }
    }
}

impl InterpMut for Label {
    type Env = CEnv;
    type Output = Number;

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        let target = env
            .clone()
            .block_map
            .get(self)
            .expect(format!("Program should have label {}, but does not", self).as_str())
            .clone();
        target.interp_(env)
    }
}

impl InterpMut for CProgram {
    type Env = CEnv;
    type Output = Number;

    fn interp(&self) -> Self::Output {
        let mut env = CEnv::new(self);
        self.interp_(&mut env)
    }

    fn interp_(&self, mut env: &mut Self::Env) -> Self::Output {
        Label!("main").interp_(&mut env)
    }
}

impl InterpMut for CTail {
    type Env = CEnv;
    type Output = Number;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CTail::Return(ret) => ret.interp_(env),
            CTail::Seq(ex, rest) => {
                ex.interp_(env);
                rest.interp_(env)
            }
        }
    }

    fn interp(&self) -> Self::Output {
        todo!()
    }
}

impl InterpMut for CStatement {
    type Env = CEnv;
    type Output = Number;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CStatement::Set(v, ex) => {
                let val = ex.interp_(env);
                env.var_map.insert(v.into(), val);
                val
            }
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

impl InterpMut for CArgument {
    type Env = CEnv;
    type Output = Number;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CArgument::Num(n) => *n,
            CArgument::Var(v) => *env.var_map.get(v).expect("Undefined variable"),
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

impl InterpMut for CExpression {
    type Env = CEnv;
    type Output = Number;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CExpression::Arg(a) => a.interp_(env),
            CExpression::Read => {
                let count = env.read_count;
                env.read_count += 1;
                count as Number
            }
            CExpression::Negate(ex) => -1 * ex.interp_(env),
            CExpression::Add(lh, rh) => lh.interp_(env) + rh.interp_(env),
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

#[cfg(test)]
mod test_cprog {
    use super::*;
    use CArgument::*;
    use CExpression::*;
    use CStatement::*;
    use CTail::*;

    type TestPrograms = Vec<(CProgram, Number)>;

    #[test]
    fn test_c0() {
        let test_progs: TestPrograms = vec![
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Arg(Num(5))),
                        Box::new(Return(Var("0".into()))),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                5,
            ),
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Arg(Num(5))),
                        Box::new(Seq(
                            Set("1".into(), Arg(Num(6))),
                            Box::new(Seq(
                                Set("2".into(), Add(Var("0".into()), Var("1".into()))),
                                Box::new(Seq(
                                    Set("3".into(), Add(Var("0".into()), Var("2".into()))),
                                    Box::new(Return(Var("3".into()))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                16,
            ),
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Arg(Num(5))),
                        Box::new(Seq(
                            Set("1".into(), Arg(Num(6))),
                            Box::new(Seq(
                                Set("2".into(), Add(Var("0".into()), Var("1".into()))),
                                Box::new(Seq(
                                    Set("3".into(), Negate(Var("2".into()))),
                                    Box::new(Return(Var("3".into()))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                -11,
            ),
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Read),
                        Box::new(Seq(
                            Set("1".into(), Read),
                            Box::new(Seq(
                                Set("2".into(), Add(Var("0".into()), Var("1".into()))),
                                Box::new(Seq(
                                    Set("3".into(), Negate(Var("2".into()))),
                                    Box::new(Return(Var("3".into()))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                -1,
            ),
        ];

        for (test_program, expected_res) in test_progs {
            let res = test_program.interp();

            assert_eq!(
                res, expected_res,
                "\nExpression {:?},  returned an invalid result, {} != {}",
                test_program, res, expected_res
            )
        }
    }

    #[test]
    #[should_panic(expected = "Undefined variable")]
    fn test_c0_undefined_variable() {
        let test_progs: TestPrograms = vec![(
            vec![(
                Label!("main"),
                Seq(
                    Set("0".into(), Arg(Num(5))),
                    Box::new(Return(Var("1".into()))),
                ),
            )]
            .into_iter()
            .collect::<CProgram>(),
            5,
        )];

        for (test_program, expected_res) in test_progs {
            let res = test_program.interp();

            assert_eq!(
                res, expected_res,
                "\nExpression {:?},  returned an invalid result, {} != {}",
                test_program, res, expected_res
            )
        }
    }
}
