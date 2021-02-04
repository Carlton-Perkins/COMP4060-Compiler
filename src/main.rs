use std::collections::HashMap;

use rand::{prelude::SliceRandom, random, seq::IteratorRandom, thread_rng};

type Var = usize;
#[derive(Debug, PartialEq, Clone)]
enum Expr {
    Num(i64),
    Read,
    Negate(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Let(Var, Box<Expr>, Box<Expr>),
    Var(Var),
}
use i64 as OType;
use Expr::*;

struct Env {
    read_count: isize,
    vars: HashMap<Var, OType>,
}

impl Env {
    fn new() -> Self {
        Env {
            read_count: 0,
            vars: HashMap::new(),
        }
    }
}

type Program<'a> = (Expr, &'a mut Env);

fn interp((expr, mut env): Program) -> OType {
    match expr {
        Num(n) => n,
        Read => {
            let res = env.read_count as i64;
            env.read_count += 1;
            res
        }
        Negate(ex) => -1 * interp((*ex, &mut env)),
        Add(lh, rh) => interp((*lh, &mut env)) + interp((*rh, &mut env)),
        Let(v, ve, be) => {
            let value = interp((*ve, env));
            env.vars.insert(v, value);
            interp((*be, env))
        }
        Var(n) => env.vars[&n],
    }
}

fn two_n(n: usize) -> Expr {
    match n {
        0 => Num(1),
        n => Add(Box::new(two_n(n - 1)), Box::new(two_n(n - 1))),
    }
}

#[derive(Clone)]
struct RandEnv {
    vars: Vec<Var>,
}

impl RandEnv {
    fn new() -> Self {
        RandEnv {vars: Vec::new()}
    }
}

fn randp(depth: usize, env: &RandEnv) -> Expr {
    type DoType = Box<dyn Fn(usize, &RandEnv) -> Expr>;
    let do_read = |_: usize, _: &RandEnv| -> Expr { Read };
    let do_num = |_: usize, _: &RandEnv| -> Expr { Num(random::<i8>() as i64) };
    let do_var = |_: usize, env: &RandEnv| -> Expr {
        let mut rng = thread_rng();
        Var(*(env.vars.choose(&mut rng).unwrap()))
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
    let do_negate = |depth: usize, env: &RandEnv| -> Expr { Negate(Box::new(randp(depth - 1, env))) };
    let do_let = |depth: usize, env: &RandEnv| -> Expr {
        let mut new_env = env.clone();
        let new_var = new_env.vars.len();
        new_env.vars.push(new_var);
        Let(new_var,Box::new(randp(depth-1, env)), Box::new(randp(depth-1, &new_env)))
    };
    let do_dn: Vec<DoType> = vec![Box::new(do_add), Box::new(do_negate), Box::new(do_let)];

    let mut rng = thread_rng();
    match depth {
        0 => {
            do_dzero.choose(&mut rng).unwrap()(0, env)
        }
        n => {
            do_dn.choose(&mut rng).unwrap()(n, env)
        }
    }
}

fn opt(e: Expr) -> Expr {
    match e {
        Num(_) => e,
        Read => e,
        Negate(ex) => {
            let o = opt(*ex);
            match o {
                Num(n) => Num(-1 * n),
                Read => Negate(Box::new(o)),
                Negate(n) => *n,
                Add(_, _) => Negate(Box::new(o)),
                n => Negate(Box::new(n)),
            }
        }
        Add(le, re) => {
            let o = (opt(*le), opt(*re));

            match o.clone() {
                (Num(l), Num(r)) => Num(l + r),
                (Num(l), Add(r1, r2)) => match *r1 {
                    Num(r) => Add(Box::new(Num(l + r)), r2),
                    _ => Add(Box::new(o.0), Box::new(o.1)),
                },
                (Add(l1, l2), Num(r)) => match *l1 {
                    Num(l) => Add(Box::new(Num(l + r)), l2),
                    _ => Add(Box::new(o.0), Box::new(o.1)),
                },
                (Add(l1, l2), Add(r1, r2)) => match (*l1, *r1) {
                    (Num(l), Num(r)) => Add(Box::new(Num(l + r)), Box::new(Add(l2, r2))),
                    _ => Add(Box::new(o.0), Box::new(o.1)),
                },

                _ => Add(Box::new(o.0), Box::new(o.1)),
            }
        }
        Let(_, _, _) => {
            unimplemented!()
        }
        Expr::Var(_) => {
            unimplemented!()
        }
    }
}

fn main() {
    // println!("{:?}", Num(5));
    // println!("{:?}", Add(Box::new(Num(5)), Box::new(Num(6))));
    // println!(
    //     "{:?}",
    //     Add(Box::new(Read), Box::new(Negate(Box::new(Num(6)))))
    // );
    // println!(
    //     "{:?}",
    //     interp((
    //         Add(Box::new(Read), Box::new(Negate(Box::new(Num(6))))),
    //         &mut Env::new()
    //     ))
    // );
    // println!("{:?}", two_n(2));
    for _ in 0..10 {
        let e = randp(4, &RandEnv::new());
        let prog = (e.clone(), &mut Env::new());
        println!("{:?} -> {}", e, interp(prog))
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    fn a_interp(expr: Expr, expect: OType) {
        let prog = (expr, &mut Env::new());
        let res = interp(prog);
        assert_eq!(res, expect);
    }

    fn a_interp_all(vec: Vec<(Expr, OType)>) {
        for (e, ex) in vec {
            a_interp(e, ex)
        }
    }

    #[test]
    fn test_r0() {
        let tests = vec![
            (Num(5), 5),
            (Num(-5), -5),
            (Add(Box::new(Num(5)), Box::new(Num(6))), 11),
            (Add(Box::new(Read), Box::new(Read)), 1),
            (Read, 0),
            (Negate(Box::new(Num(5))), -5),
            (
                Add(Box::new(Num(5)), Box::new(Negate(Box::new(Num(6))))),
                -1,
            ),
            (Add(Box::new(Read), Box::new(Negate(Box::new(Num(6))))), -6),
            (
                Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(6))))))),
                -6,
            ),
            (
                Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(0))))))),
                0,
            ),
        ];

        a_interp_all(tests);
    }

    #[test]
    fn test_r1() {
        let tests = vec![
            (Let(0, Box::new(Num(0)), Box::new(Read)), 0),
            (
                Let(
                    0,
                    Box::new(Num(0)),
                    Box::new(Let(0, Box::new(Num(1)), Box::new(Var(0)))),
                ),
                1,
            ),
            (
                Let(
                    0,
                    Box::new(Num(4)),
                    Box::new(Let(
                        1,
                        Box::new(Num(5)),
                        Box::new(Add(Box::new(Var(0)), Box::new(Var(1)))),
                    )),
                ),
                9,
            ),
            (
                Let(
                    0,
                    Box::new(Read),
                    Box::new(Let(
                        1,
                        Box::new(Read),
                        Box::new(Add(Box::new(Var(0)), Box::new(Var(1)))),
                    )),
                ),
                1,
            ),
        ];

        a_interp_all(tests);
    }

    #[test]
    fn test_two_n() {
        a_interp(two_n(0), 1);
        a_interp(two_n(1), 2);
        a_interp(two_n(2), 4);
        a_interp(two_n(3), 8);
        a_interp(two_n(4), 16);
        a_interp(two_n(5), 32);
    }

    #[test]
    #[ignore = "Slow"]
    fn test_randp() {
        for depth in 0..10 {
            for _ in 0..100 {
                let e = randp(depth, &RandEnv::new());
                let prog = (e.clone(), &mut Env::new());
                println!("{:?} -> {}", e, interp(prog));
            }
        }
    }

    #[test]
    fn test_opt() {
        let test_expr = vec![
            (Num(5), Num(5), 5),
            (Negate(Box::new(Num(5))), Num(-5), -5),
            (Negate(Box::new(Read)), Negate(Box::new(Read)), 0),
            (Add(Box::new(Num(3)), Box::new(Num(2))), Num(5), 5),
            (
                Add(Box::new(Num(3)), Box::new(Read)),
                Add(Box::new(Num(3)), Box::new(Read)),
                3,
            ),
        ];

        for (e, expect_opt, expect_res) in test_expr {
            let e_res = interp((e.clone(), &mut Env::new()));
            let opt = opt(e.clone());
            let opt_res = interp((opt.clone(), &mut Env::new()));

            assert_eq!(opt, expect_opt);
            assert_eq!(e_res, expect_res);
            assert_eq!(e_res, opt_res);
        }
    }

    #[test]
    #[ignore = "Slow"]
    fn test_randp_opt() {
        for depth in 0..20 {
            for _ in 0..100 {
                let e = randp(depth, &RandEnv::new());
                let e_res = interp((e.clone(), &mut Env::new()));
                let opt = opt(e.clone());
                let opt_res = interp((opt.clone(), &mut Env::new()));

                assert_eq!(e_res, opt_res, "'{:?}' does not equal '{:?}'", e, opt);
            }
        }
    }
}
