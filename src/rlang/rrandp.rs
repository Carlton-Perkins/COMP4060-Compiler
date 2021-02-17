use crate::rlang::Expr::*;
use crate::rlang::{Expr, Variable};
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

pub fn randp(depth: usize, env: &RandEnv) -> Expr {
    type DoType = Box<dyn Fn(usize, &RandEnv) -> Expr>;
    let do_read = |_: usize, _: &RandEnv| -> Expr { Read };
    let do_num = |_: usize, _: &RandEnv| -> Expr { Num(random::<i8>() as i64) };
    let do_var = |_: usize, env: &RandEnv| -> Expr {
        let mut rng = thread_rng();
        Var((env.vars.choose(&mut rng).unwrap()).to_string())
    };
    let mut do_dzero: Vec<DoType> = vec![Box::new(do_read), Box::new(do_num)];
    if env.vars.len() > 0 {
        do_dzero.push(Box::new(do_var))
    }

    let do_add = |depth: usize, env: &RandEnv| -> Expr {
        Add(
            Box::new(randp(depth - 1, env)),
            Box::new(randp(depth - 1, env)),
        )
    };
    let do_negate =
        |depth: usize, env: &RandEnv| -> Expr { Negate(Box::new(randp(depth - 1, env))) };
    let do_let = |depth: usize, env: &RandEnv| -> Expr {
        let mut new_env = env.clone();
        let new_var = new_env.vars.len().to_string();
        new_env.vars.push(new_var.clone());
        Let(
            new_var,
            Box::new(randp(depth - 1, env)),
            Box::new(randp(depth - 1, &new_env)),
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
    use super::*;
    use crate::rlang::{Env, InterpMut};

    #[test]
    #[ignore = "Slow"]
    fn test_randp() {
        for depth in 0..10 {
            for _ in 0..100 {
                let e = randp(depth, &RandEnv::new());
                println!("{:?} -> {}", e, e.interp(&mut Env::new()));
            }
        }
    }
}
