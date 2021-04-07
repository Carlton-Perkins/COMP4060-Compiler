use crate::common::{
    traits::InterpMut,
    types::{Answer, Label, Number, Variable, CMP},
};
use std::collections::HashMap;

type CLabelMapping = HashMap<Label, CTail>;
pub type CProgram = CLabelMapping;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CArgument {
    CNum(Number),
    CVar(Variable),
    CBool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CExpression {
    Arg(CArgument),
    Read,
    Negate(CArgument),
    Add(CArgument, CArgument),
    Not(CArgument),
    Cmp(CMP, CArgument, CArgument),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CStatement {
    Set(Variable, CExpression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CTail {
    Return(CArgument),
    Seq(CStatement, Box<CTail>),
    Goto(Label),
    GotoIf(CMP, CArgument, CArgument, Label, Label),
}

#[derive(Clone)]
pub struct CEnv {
    read_count: usize,
    block_map: CLabelMapping,
    var_map: HashMap<Variable, Answer>,
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
    type Output = Answer;

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
    type Output = Answer;

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
    type Output = Answer;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CTail::Return(ret) => ret.interp_(env),
            CTail::Seq(ex, rest) => {
                ex.interp_(env);
                rest.interp_(env)
            }
            CTail::Goto(_) => {
                todo!("C0 -> C1")
            }
            CTail::GotoIf(_, _, _, _, _) => {
                todo!("C0 -> C1")
            }
        }
    }

    fn interp(&self) -> Self::Output {
        todo!()
    }
}

impl InterpMut for CStatement {
    type Env = CEnv;
    type Output = Answer;

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CStatement::Set(v, ex) => {
                let val = ex.interp_(env);
                env.var_map.insert(v.into(), val);
                val
            }
        }
    }
}

impl InterpMut for CArgument {
    type Env = CEnv;
    type Output = Answer;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CArgument::CNum(n) => Answer::S64(*n),
            CArgument::CVar(v) => *env.var_map.get(v).expect("Undefined variable"),
            CArgument::CBool(_) => {
                todo!("C0 -> C1")
            }
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

impl InterpMut for CExpression {
    type Env = CEnv;
    type Output = Answer;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        match self {
            CExpression::Arg(a) => a.interp_(env),
            CExpression::Read => {
                let count = env.read_count;
                env.read_count += 1;
                count.into()
            }
            CExpression::Negate(ex) => Answer::S64(-1) * ex.interp_(env),
            CExpression::Add(lh, rh) => lh.interp_(env) + rh.interp_(env),
            CExpression::Not(_) => {
                todo!("C0 -> C1")
            }
            CExpression::Cmp(_, _, _) => {
                todo!("C0 -> C1")
            }
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

#[cfg(test)]
mod test_cprog {
    use super::*;
    use pretty_assertions::assert_eq;
    use Answer::*;
    use CArgument::*;
    use CExpression::*;
    use CStatement::*;
    use CTail::*;

    type TestPrograms = Vec<(CProgram, Answer)>;

    #[test]
    fn test_c0() {
        let test_progs: TestPrograms = vec![
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Arg(CNum(5))),
                        Box::new(Return(CVar("0".into()))),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                S64(5),
            ),
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Arg(CNum(5))),
                        Box::new(Seq(
                            Set("1".into(), Arg(CNum(6))),
                            Box::new(Seq(
                                Set("2".into(), Add(CVar("0".into()), CVar("1".into()))),
                                Box::new(Seq(
                                    Set("3".into(), Add(CVar("0".into()), CVar("2".into()))),
                                    Box::new(Return(CVar("3".into()))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                S64(16),
            ),
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Arg(CNum(5))),
                        Box::new(Seq(
                            Set("1".into(), Arg(CNum(6))),
                            Box::new(Seq(
                                Set("2".into(), Add(CVar("0".into()), CVar("1".into()))),
                                Box::new(Seq(
                                    Set("3".into(), Negate(CVar("2".into()))),
                                    Box::new(Return(CVar("3".into()))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                S64(-11),
            ),
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Read),
                        Box::new(Seq(
                            Set("1".into(), Read),
                            Box::new(Seq(
                                Set("2".into(), Add(CVar("0".into()), CVar("1".into()))),
                                Box::new(Seq(
                                    Set("3".into(), Negate(CVar("2".into()))),
                                    Box::new(Return(CVar("3".into()))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<CProgram>(),
                S64(-1),
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
    fn test_c1() {
        let test_progs: TestPrograms = vec![
            (
                CProgram!(CTail!(
                    Label!("main"),
                    CSeq!(CSet!("0", Arg(CBool(true))), Return(CVar!("0")))
                )),
                Bool(true),
            ),
            (
                CProgram!(CTail!(
                    Label!("main"),
                    CSeq!(
                        CSet!("0", Arg(CBool(true))),
                        CSeq!(CSet!("1", Not(CVar!("0"))), Return(CVar!("1")))
                    )
                )),
                Bool(false),
            ),
            (
                CProgram!(CTail!(
                    Label!("main"),
                    CSeq!(
                        CSet!("0", Arg(CNum(4))),
                        CSeq!(
                            CSet!("1", Arg(CNum(4))),
                            CSeq!(
                                CSet!("2", Cmp(CMP::EQ, CVar!("0"), CVar!("1"))),
                                Return(CVar!("2"))
                            )
                        )
                    )
                )),
                Bool(true),
            ),
            (
                CProgram!(
                    CTail!(Label!("main"), Goto("second".into())),
                    CTail!(Label!("second"), Goto("third".into())),
                    CTail!(Label!("third"), Goto("end".into())),
                    CTail!(Label!("end"), Return(CNum(5))),
                ),
                S64(5),
            ),
            (
                CProgram!(
                    CTail!(Label!("main"), Goto("second".into())),
                    CTail!(
                        Label!("second"),
                        GotoIf(CMP::EQ, CNum(5), CNum(5), "third".into(), "forth".into())
                    ),
                    CTail!(
                        Label!("third"),
                        CSeq!(CSet!("x", Arg(CNum(42))), Goto("end".into()))
                    ),
                    CTail!(
                        Label!("forth"),
                        CSeq!(CSet!("x", Arg(CNum(-24))), Goto("end".into()))
                    ),
                    CTail!(Label!("end"), Return(CVar!("x"))),
                ),
                S64(42),
            ),
            (
                CProgram!(
                    CTail!(Label!("main"), Goto("second".into())),
                    CTail!(
                        Label!("second"),
                        GotoIf(CMP::LT, CNum(5), CNum(4), "third".into(), "forth".into())
                    ),
                    CTail!(
                        Label!("third"),
                        CSeq!(CSet!("x", Arg(CNum(42))), Goto("end".into()))
                    ),
                    CTail!(
                        Label!("forth"),
                        CSeq!(CSet!("x", Arg(CNum(-24))), Goto("end".into()))
                    ),
                    CTail!(Label!("end"), Return(CVar!("x"))),
                ),
                S64(-24),
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
                    Set("0".into(), Arg(CNum(5))),
                    Box::new(Return(CVar("1".into()))),
                ),
            )]
            .into_iter()
            .collect::<CProgram>(),
            S64(5),
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
