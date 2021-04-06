use std::collections::HashMap;

use crate::{
    common::types::Variable,
    rlang::{RExpr, RExpr::*, Type, RCMP},
};
use rand::prelude::*;

const RAND_TYPES: &[Type] = &[Type::S64, Type::Bool];
const RAND_CMP: &[RCMP] = &[RCMP::EQ, RCMP::LT, RCMP::LEQ, RCMP::GEQ, RCMP::GT];
#[derive(Clone)]
pub struct RandEnv {
    var_c: usize,
    vars: HashMap<Type, Vec<Variable>>,
}

impl RandEnv {
    pub fn new() -> Self {
        let mut vars = HashMap::new();
        RAND_TYPES.into_iter().for_each(|x| {
            vars.insert(*x, Vec::<Variable>::new());
        });
        RandEnv { vars, var_c: 0 }
    }
}

pub fn randp(depth: usize) -> RExpr {
    let t = rand_type();
    println!("RandP Type: {:?}", t);
    randp_(depth, &t, &mut RandEnv::new())
}

fn randp_(depth: usize, ty: &Type, env: &mut RandEnv) -> RExpr {
    type DoType = Box<dyn Fn(usize, &Type, &mut RandEnv) -> RExpr>;
    let do_read = |_: usize, _: &Type, _: &mut RandEnv| -> RExpr { RRead };
    let do_num = |_: usize, _: &Type, _: &mut RandEnv| -> RExpr { RNum(random::<i8>() as i64) };
    let do_var = |_: usize, ty: &Type, env: &mut RandEnv| -> RExpr {
        let mut rng = thread_rng();
        let vars = env.vars.get(&ty).unwrap();
        RVar((vars.choose(&mut rng).unwrap()).to_string())
    };
    let do_bool = |_: usize, _: &Type, _: &mut RandEnv| -> RExpr { RBool(random::<bool>()) };

    let mut do_dzero: Vec<DoType> = match ty {
        Type::S64 => {
            vec![Box::new(do_read), Box::new(do_num)]
        }
        Type::Bool => {
            vec![Box::new(do_bool)]
        }
    };
    if env.vars.get(ty).unwrap().len() > 0 {
        do_dzero.push(Box::new(do_var))
    }

    let do_add = |depth: usize, _: &Type, env: &mut RandEnv| -> RExpr {
        RAdd(
            Box::new(randp_(depth - 1, &Type::S64, env)),
            Box::new(randp_(depth - 1, &Type::S64, env)),
        )
    };
    let do_negate = |depth: usize, _: &Type, env: &mut RandEnv| -> RExpr {
        RNegate(Box::new(randp_(depth - 1, &Type::S64, env)))
    };
    let do_let = |depth: usize, ty: &Type, env: &mut RandEnv| -> RExpr {
        let new_var = env.var_c.to_string();
        env.var_c += 1;
        let mut new_env = env.clone();
        let mut new_var_vec = new_env.vars.get(&ty).unwrap().clone();
        new_var_vec.push(new_var.clone());
        new_env.vars.insert(*ty, new_var_vec);
        RLet(
            new_var,
            Box::new(randp_(depth - 1, ty, env)),
            Box::new(randp_(depth - 1, ty, &mut new_env)),
        )
    };
    let do_cmp = |depth: usize, _: &Type, env: &mut RandEnv| -> RExpr {
        let cmp = rand_cmp();
        let inner_type = rand_type();
        let lh = randp_(depth - 1, &inner_type, env);
        let rh = randp_(depth - 1, &inner_type, env);
        RCmp(cmp, Box::new(lh), Box::new(rh))
    };
    let do_if = |depth: usize, ty: &Type, env: &mut RandEnv| -> RExpr {
        RIf!(
            randp_(depth - 1, &Type::Bool, env),
            randp_(depth - 1, ty, env),
            randp_(depth - 1, ty, env)
        )
    };

    let do_dn: Vec<DoType> = match ty {
        Type::S64 => {
            vec![
                Box::new(do_add),
                Box::new(do_negate),
                Box::new(do_let),
                Box::new(do_if),
            ]
        }
        Type::Bool => {
            vec![Box::new(do_let), Box::new(do_if), Box::new(do_cmp)]
        }
    };

    let mut rng = thread_rng();
    match depth {
        0 => do_dzero.choose(&mut rng).unwrap()(0, ty, env),
        n => do_dn.choose(&mut rng).unwrap()(n, ty, env),
    }
}

fn rand_type() -> Type {
    *RAND_TYPES.choose(&mut thread_rng()).unwrap()
}

fn rand_cmp() -> RCMP {
    *RAND_CMP.choose(&mut thread_rng()).unwrap()
}

#[cfg(test)]
mod test_rrandp {
    use super::*;
    use crate::{
        clang::{SelectInstruction, UncoverLocals},
        common::traits::InterpMut,
        rlang::{ExplicateControl, ResolveComplex, Uniquify},
        xlang::{AssignRegisters, CompileAndRun, GraphAllocator, PatchInstructions, XInterpMut},
    };
    use itertools::Itertools;
    use pretty_assertions::assert_eq;
    use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

    #[test]
    #[cfg_attr(tarpaulin, ignore)]
    fn test_randp() {
        let max_depth = 10;
        let iter_per_depth = 100;

        let jobs: Vec<(usize, usize)> = (0..max_depth)
            .cartesian_product(0..iter_per_depth)
            .collect();
        jobs.par_iter()
            .for_each(|(depth, _)| test_random_program(*depth));
    }

    fn test_random_program(depth: usize) {
        // RLang
        let e = randp(depth);
        // println!("Program: {:?}", e);
        let e_ret = e.interp().unwrap();

        let u = e.uniquify();
        let u_ret = u.interp().unwrap();
        assert_eq!(e_ret, u_ret, "Randp Failure at  uniquify ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\n Generated return: {}\n", e, e_ret, u, u_ret);

        let rco = u.resolve_complex();
        let rco_ret = rco.interp().unwrap();
        assert_eq!(e_ret, rco_ret, "Randp Failure at rco ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\n Generated return: {}\n", e, e_ret, rco, rco_ret);

        let econ = rco.explicate_control();
        let econ_ret = econ.interp();
        assert_eq!(e_ret, econ_ret, "Randp Failure at econ ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\n Generated return: {}\n", e, e_ret, econ, econ_ret);

        // CLang
        let (ul, local_info) = econ.uncover_locals();
        let ul_ret = ul.interp();
        assert_eq!(e_ret, ul_ret, "Randp Failure at uncover locals ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\nLocals: {:?}\n Generated return: {}\n", e, e_ret, ul, local_info, ul_ret);

        let sel_inst = ul.select_instr();
        let sel_inst_ret = sel_inst.interp();
        assert_eq!(e_ret, sel_inst_ret, "Randp Failure at selinst ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\n Generated return: {}\n", e, e_ret, sel_inst, sel_inst_ret);

        // XLang
        let asn = sel_inst.asn_registers(&local_info, GraphAllocator {});
        let asn_ret = asn.interp();
        assert_eq!(e_ret, asn_ret, "Randp Failure at asn ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\n Generated return: {}\n", e, e_ret, asn, asn_ret);

        let patch = asn.patch();
        let patch_ret = patch.interp();
        assert_eq!(e_ret, patch_ret, "Randp Failure at patch ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\n Generated return: {}\n", e, e_ret, patch, patch_ret);

        let sys_ret = patch.run();
        assert_eq!(e_ret, sys_ret, "Randp Failure at sys ->\nProgram: {:?}\nExpected return: {}\nGenerated Program: {:?}\n Generated return: {}\n", e, e_ret, patch, sys_ret);
    }
}
