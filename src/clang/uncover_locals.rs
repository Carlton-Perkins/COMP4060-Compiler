use crate::{
    clang::{CArgument, CExpression, CProgram, CStatement, CTail},
    common::types::Variable,
};
use std::collections::HashSet;
use std::iter::FromIterator;

pub type LocalsInfo = HashSet<Variable>;
pub type CProgramInfo = (CProgram, LocalsInfo);

pub trait UncoverLocals {
    fn uncover_locals(&self) -> CProgramInfo;
}

trait LocateVars {
    fn locate_vars(&self) -> LocalsInfo;
}

impl UncoverLocals for CProgram {
    fn uncover_locals(&self) -> CProgramInfo {
        let locals = self
            .get(&Label!("main").clone())
            .expect(format!("No main label in program {:?}", self).as_str());
        (self.clone(), locals.locate_vars())
    }
}

impl LocateVars for CTail {
    fn locate_vars(&self) -> LocalsInfo {
        match self {
            CTail::Return(e) => e.locate_vars(),
            CTail::Seq(e, tail) => e
                .locate_vars()
                .union(&tail.locate_vars())
                .map(|x| x.clone())
                .collect(),
        }
    }
}

impl LocateVars for CArgument {
    fn locate_vars(&self) -> LocalsInfo {
        match self {
            CArgument::Num(_) => LocalsInfo::new(),
            CArgument::Var(var) => LocalsInfo::from_iter(vec![var.clone()]),
        }
    }
}

impl LocateVars for CStatement {
    fn locate_vars(&self) -> LocalsInfo {
        match self {
            CStatement::Set(var, ex) => LocalsInfo::from_iter(vec![var.clone()])
                .union(&ex.locate_vars())
                .map(|x| x.clone())
                .collect(),
        }
    }
}

impl LocateVars for CExpression {
    fn locate_vars(&self) -> LocalsInfo {
        match self {
            CExpression::Arg(e) => e.locate_vars(),
            CExpression::Read => LocalsInfo::new(),
            CExpression::Negate(e) => e.locate_vars(),
            CExpression::Add(lh, rh) => lh
                .locate_vars()
                .union(&rh.locate_vars())
                .map(|x| x.clone())
                .collect(),
        }
    }
}

#[cfg(test)]
mod test_uncover_locals {
    use super::*;
    use crate::clang::*;
    use pretty_assertions::assert_eq;
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
