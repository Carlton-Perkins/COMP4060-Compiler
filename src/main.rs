use rand::random;

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
}

impl Env {
    fn new() -> Self {
        Env { read_count: 0 }
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
        Let(_, _, _) => unimplemented!(),
        Var(_) => unimplemented!(),
    }
}

fn two_n(n: usize) -> Expr {
    match n {
        0 => Num(1),
        n => Add(Box::new(two_n(n - 1)), Box::new(two_n(n - 1))),
    }
}

fn randp(depth: usize) -> Expr {
    match depth {
        0 => {
            if random() {
                Read
            } else {
                // Generate much smaller numbers for now to not overflow
                Num(random::<i8>() as i64)
            }
        }
        n => {
            if random() {
                Add(Box::new(randp(n - 1)), Box::new(randp(n - 1)))
            } else {
                Negate(Box::new(randp(n - 1)))
            }
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
        let e = randp(5);
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
        for depth in 0..20 {
            for _ in 0..1000 {
                let e = randp(depth);
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
                let e = randp(depth);
                let e_res = interp((e.clone(), &mut Env::new()));
                let opt = opt(e.clone());
                let opt_res = interp((opt.clone(), &mut Env::new()));

                assert_eq!(e_res, opt_res, "'{:?}' does not equal '{:?}'", e, opt);
            }
        }
    }
}
