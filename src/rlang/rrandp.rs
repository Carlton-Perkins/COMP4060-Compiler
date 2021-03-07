use crate::{
    common::types::Variable,
    rlang::{RExpr, RExpr::*},
};
use rand::prelude::*;

#[derive(Clone)]
pub struct RandEnv {
    vars: Vec<Variable>,
}

impl RandEnv {
    pub fn new() -> Self {
        RandEnv { vars: Vec::new() }
    }
}

pub fn randp(depth: usize) -> RExpr {
    randp_(depth, &RandEnv::new())
}

fn randp_(depth: usize, env: &RandEnv) -> RExpr {
    type DoType = Box<dyn Fn(usize, &RandEnv) -> RExpr>;
    let do_read = |_: usize, _: &RandEnv| -> RExpr { RRead };
    let do_num = |_: usize, _: &RandEnv| -> RExpr { RNum(random::<i8>() as i64) };
    let do_var = |_: usize, env: &RandEnv| -> RExpr {
        let mut rng = thread_rng();
        RVar((env.vars.choose(&mut rng).unwrap()).to_string())
    };
    let mut do_dzero: Vec<DoType> = vec![Box::new(do_read), Box::new(do_num)];
    if env.vars.len() > 0 {
        do_dzero.push(Box::new(do_var))
    }

    let do_add = |depth: usize, env: &RandEnv| -> RExpr {
        RAdd(
            Box::new(randp_(depth - 1, env)),
            Box::new(randp_(depth - 1, env)),
        )
    };
    let do_negate =
        |depth: usize, env: &RandEnv| -> RExpr { RNegate(Box::new(randp_(depth - 1, env))) };
    let do_let = |depth: usize, env: &RandEnv| -> RExpr {
        let mut new_env = env.clone();
        let new_var = new_env.vars.len().to_string();
        new_env.vars.push(new_var.clone());
        RLet(
            new_var,
            Box::new(randp_(depth - 1, env)),
            Box::new(randp_(depth - 1, &new_env)),
        )
    };
    let do_dn: Vec<DoType> = vec![Box::new(do_add), Box::new(do_negate), Box::new(do_let)];

    let mut rng = thread_rng();
    match depth {
        0 => do_dzero.choose(&mut rng).unwrap()(0, env),
        n => do_dn.choose(&mut rng).unwrap()(n, env),
    }
}

#[cfg(test)]
mod test_rrandp {
    use itertools::Itertools;
    use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

    use super::*;
    use crate::{
        clang::{SelectInstruction, UncoverLocals},
        common::traits::InterpMut,
        rlang::{ExplicateControl, ResolveComplex, Uniquify},
        xlang::{AssignHomes, CompileAndRun, PatchInstructions, XInterpMut},
    };

    #[test]
    fn test_randp() {
        let max_depth = 10;
        let iter_per_depth = 10;

        let jobs: Vec<(usize, usize)> = (0..max_depth)
            .cartesian_product(0..iter_per_depth)
            .collect();
        jobs.par_iter()
            .for_each(|(depth, _)| test_random_program(*depth));
    }

    fn test_random_program(depth: usize) {
        // RLang
        let e = randp(depth);
        println!("Program: {:?}", e);
        let e_ret = e.interp().unwrap();

        let u = e.uniquify();
        let u_ret = u.interp().unwrap();
        assert_eq!(e_ret, u_ret);

        let rco = u.resolve_complex();
        let rco_ret = rco.interp().unwrap();
        assert_eq!(e_ret, rco_ret);

        let econ = rco.explicate_control();
        let econ_ret = econ.interp();
        assert_eq!(e_ret, econ_ret);

        // CLang
        let (ul, local_info) = econ.uncover_locals();
        let ul_ret = ul.interp();
        assert_eq!(e_ret, ul_ret);

        let sel_inst = ul.select_instr();
        let sel_inst_ret = sel_inst.interp();
        assert_eq!(e_ret, sel_inst_ret);

        // XLang
        let asn = sel_inst.asn_homes(&local_info);
        let asn_ret = asn.interp();
        assert_eq!(e_ret, asn_ret);

        let patch = asn.patch();
        let patch_ret = patch.interp();
        assert_eq!(e_ret, patch_ret);

        let sys_res = patch.run();
        assert_eq!(e_ret, sys_res);
    }
}
