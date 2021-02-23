use std::collections::HashSet;

use crate::{
    clang::{CArgument, CEnv, CExpression, CProgram, CStatement, CTail},
    common::types::Variable,
};

pub type LocalsInfo = HashSet<Variable>;
pub type CProgramInfo = (CProgram, LocalsInfo);

pub trait UncoverLocals {
    fn uncover_locals(&self) -> CProgramInfo;
}

impl UncoverLocals for CProgram {
    fn uncover_locals(&self) -> CProgramInfo {
        unimplemented!()
    }
}

#[cfg(test)]
mod test_uncover_locals {
    use super::*;
    use crate::clang::*;
    use CArgument::*;
    use CExpression::*;
    use CStatement::*;
    use CTail::*;

    type TestPrograms = Vec<(CProgram, LocalsInfo)>;

    #[test]
    fn test_ucl() {
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
                vec!["0"].into_iter().map(|x| x.into()).collect(),
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
                vec!["0", "1", "2", "3"]
                    .into_iter()
                    .map(|x| x.into())
                    .collect(),
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
                vec!["0", "1", "2", "3"]
                    .into_iter()
                    .map(|x| x.into())
                    .collect(),
            ),
            (
                vec![(
                    Label!("main"),
                    Seq(
                        Set("0".into(), Read()),
                        Box::new(Seq(
                            Set("1".into(), Read()),
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
                vec!["0", "1", "2", "3"]
                    .into_iter()
                    .map(|x| x.into())
                    .collect(),
            ),
        ];

        for (prog, expected_info) in test_progs {
            let (_, info) = prog.uncover_locals();
            assert_eq!(
                info, expected_info,
                "Incorrect variables collected, {:?} -> {:?} should have been {:?}",
                prog, info, expected_info
            );
        }
    }
}
