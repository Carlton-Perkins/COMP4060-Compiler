use crate::{common::types::Variable, rlang::RProgram};
use std::collections::HashMap;

use super::Expr;

pub struct RCEnv {
    mapping: HashMap<Variable, Expr>,
}

pub trait ResolveComplex {
    fn resolve_complex(&self, env: &RCEnv) -> Self;
}

impl RCEnv {
    fn new() -> Self {
        RCEnv {
            mapping: HashMap::new(),
        }
    }
}

impl ResolveComplex for RProgram {
    fn resolve_complex(&self, env: &RCEnv) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod test_rco {
    use super::*;
    use crate::common::{traits::InterpMut, types::Number};
    use crate::rlang::Expr::*;
    use crate::rlang::REnv;

    type Test = (RProgram, RProgram, Number);
    type Tests = Vec<Test>;

    #[test]
    fn test_rco() {
        let tests: Tests = vec![
            (Num(5), Num(5), 5),
            (
                Add(
                    Box::new(Add(Box::new(Num(3)), Box::new(Num(6)))),
                    Box::new(Num(2)),
                ),
                Let(
                    "r0".into(),
                    Box::new(Add(Box::new(Num(3)), Box::new(Num(6)))),
                    Box::new(Let(
                        "r1".into(),
                        Box::new(Add(Box::new(Var("r0".into())), Box::new(Num(2)))),
                        Box::new(Var("r1".into())),
                    )),
                ),
                11,
            ),
            (
                Add!(Add!(Num(3), Num(6)), Num(2)),
                Let!(
                    "r0",
                    Add!(Num(3), Num(6)),
                    Let!("r1", Add!(Var!("r0"), Num(2)), Var!("r1"))
                ),
                11,
            ),
            (
                Add(
                    Box::new(Add(Box::new(Num(3)), Box::new(Num(6)))),
                    Box::new(Negate(Box::new(Add(Box::new(Read), Box::new(Num(1)))))),
                ),
                Let(
                    "r0".into(),
                    Box::new(Add(Box::new(Num(3)), Box::new(Num(6)))),
                    Box::new(Let(
                        "r1".into(),
                        Box::new(Add(Box::new(Var("r0".into())), Box::new(Num(2)))),
                        Box::new(Var("r1".into())),
                    )),
                ),
                11,
            ),
            (
                Add!(Add!(Num(3), Num(6)), Negate!(Add!(Read, Num(2)))),
                Let!(
                    "r0",
                    Add!(Num(3), Num(6)),
                    Let!(
                        "r1",
                        Add!(Var!("r0"), Num(2)),
                        Let!("r2", Add!(Read, Num(2)), Var!("r2"))
                    )
                ),
                7,
            ),
        ];

        for (start, expected_rco, expected_res) in tests {
            let start_res = start.interp(&mut REnv::new());
            let rco = start.resolve_complex(&RCEnv::new());
            let rco_res = rco.interp(&mut REnv::new());

            assert_eq!(
                start_res, expected_res,
                "Start RProgram {:?} does not match expected result {} and returned {} instead",
                start, expected_res, start_res
            );
            assert_eq!(
                rco, expected_rco,
                "RCO'd program {:?} does not match expected program {:?}, Started with {:?}",
                rco, expected_rco, start
            );
            assert_eq!(rco_res, expected_res,
                "RCO'd program {:?} does not output the same result as the input program {:?}, {} != {}",
                rco, start, rco_res, expected_res
            );
        }
    }
}
