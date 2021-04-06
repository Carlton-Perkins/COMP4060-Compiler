use super::RProgram;
use crate::common::types::Variable;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
enum Type {
    S64,
    Bool,
}

#[derive(Debug, PartialEq, Eq)]
struct TypeCheckError {}

struct TypeCheckEnv {
    var_type: HashMap<Variable, Type>,
}

trait TypeCheck {
    fn typec(&self) -> Result<Type, TypeCheckError>;
}

impl TypeCheck for RProgram {
    fn typec(&self) -> Result<Type, TypeCheckError> {
        todo!()
    }
}

#[cfg(test)]
mod test_typecheck {
    use super::Type::*;
    use super::*;
    use crate::rlang::RExpr::*;

    #[test]
    fn test_typec_r2() {
        let progs = vec![
            (RNum(5), Ok(S64)),
            (RBool(true), Ok(Bool)),
            (RAdd!(RNum(5), RNum(6)), Ok(S64)),
            (RAdd!(RNum(5), RBool(true)), Err(TypeCheckError {})),
            (RRead, Ok(S64)),
            (RNegate!(RNum(5)), Ok(S64)),
            (RNegate!(RBool(true)), Err(TypeCheckError {})),
            (REQ!(RNum(5), RNum(5)), Ok(Bool)),
            (REQ!(RNum(5), RBool(true)), Err(TypeCheckError {})),
            (RIf!(RTrue!(), RNum(5), RNum(2)), Ok(S64)),
            (RIf!(RTrue!(), RFalse!(), RTrue!()), Ok(Bool)),
            (RIf!(RTrue!(), RFalse!(), RNum(5)), Err(TypeCheckError {})),
            (RLet!("x", RNum(5), RVar!("x")), Ok(S64)),
            (RLet!("x", RBool(true), RVar!("x")), Ok(Bool)),
        ];

        for (prog, expected_type) in progs {
            let tr = prog.typec();
            assert_eq!(tr, expected_type);
        }
    }
}
