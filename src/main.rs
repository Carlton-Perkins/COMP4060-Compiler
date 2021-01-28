use rand::random;

#[derive(Debug, PartialEq, Clone)]
enum Expr {
    Num(i64),
    Read,
    Negate(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
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
        },
        n => {
            if random() {
                Add(Box::new(randp(n - 1)), Box::new(randp(n - 1)))
            } else {
                Negate(Box::new(randp(n -1)))
            }
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
        println!("{:?} -> {}", e ,interp(prog))
    }
    
}



#[cfg(test)]
mod tests {
    use super::*;

    fn a_interp(expr: Expr, expect: OType) {
        let prog = (expr, &mut Env::new());
        let res = interp(prog);
        assert_eq!(res, expect);
    }

    #[test]
    fn test_basic() {
        a_interp(Num(5), 5);
        a_interp(Num(-5), -5);
        a_interp(Add(Box::new(Num(5)), Box::new(Num(6))), 11);
        a_interp(Add(Box::new(Read), Box::new(Read)), 1);
        a_interp(Read, 0);
        a_interp(Negate(Box::new(Num(5))), -5);
        a_interp(
            Add(Box::new(Num(5)), Box::new(Negate(Box::new(Num(6))))),
            -1,
        );
        a_interp(Add(Box::new(Read), Box::new(Negate(Box::new(Num(6))))), -6);
        a_interp(
            Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(6))))))),
            -6,
        );
        a_interp(
            Negate(Box::new(Negate(Box::new(Negate(Box::new(Num(0))))))),
            0,
        );
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
}
