use crate::{
    common::types::Variable,
    rlang::{RExpr::*, RProgram},
};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    fn typec_(&self, env: &mut TypeCheckEnv) -> Result<Type, TypeCheckError>;
}

impl TypeCheck for RProgram {
    fn typec(&self) -> Result<Type, TypeCheckError> {
        self.typec_(&mut TypeCheckEnv::new())
    }

    fn typec_(&self, env: &mut TypeCheckEnv) -> Result<Type, TypeCheckError> {
        use Type::*;
        match self {
            RNum(_) => Ok(S64),
            RRead => Ok(S64),
            RNegate(e) => match e.typec_(env)? {
                S64 => Ok(S64),
                Bool => Err(TypeCheckError {}),
            },
            RAdd(l, r) => {
                let lt = l.typec_(env)?;
                let rt = r.typec_(env)?;
                match lt == rt {
                    true => Ok(lt),
                    false => Err(TypeCheckError {}),
                }
            }
            RLet(c, ve, be) => {
                let vt = ve.typec_(env)?;
                env.var_type.insert(c.clone(), vt);
                be.typec_(env)
            }
            RVar(v) => Ok(env
                .var_type
                .get(v)
                .expect("Typec: Unbound variable")
                .clone()),
            RBool(_) => Ok(Bool),
            RCmp(_, l, r) => {
                let lt = l.typec_(env)?;
                let rt = r.typec_(env)?;
                match lt == rt {
                    true => Ok(Bool),
                    false => Err(TypeCheckError {}),
                }
            }
            RIf(c, t, f) => {
                if c.typec_(env)? != Bool {
                    Err(TypeCheckError {})
                } else {
                    let tt = t.typec_(env)?;
                    let ft = f.typec_(env)?;
                    match tt == ft {
                        true => Ok(tt),
                        false => Err(TypeCheckError {}),
                    }
                }
            }
        }
    }
}

impl TypeCheckEnv {
    fn new() -> Self {
        TypeCheckEnv {
            var_type: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod test_typecheck {
    use super::Type::*;
    use super::*;

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
            println!("{:?}", prog);
            assert_eq!(tr, expected_type);
        }
    }
}
