#[derive(Debug, PartialEq, Copy, Clone)]
enum Expr<'a> {
    Num(isize),
    Read,
    Negate(&'a Expr<'a>),
    Add(&'a Expr<'a>, &'a Expr<'a>),
}
use Expr::*;
use isize as OType;

struct Env {
    read_count: isize,
}

impl Env {
    fn new() -> Self {
        Env{read_count: 0}    
    }
}

type Program<'a> = (Expr<'a>, &'a mut Env);

fn interp((expr, mut env): Program) -> OType {
    match expr {
        Num(n) => { n }
        Read => { let res = env.read_count; env.read_count += 1; res }
        Negate(ex) => { -1 * interp((*ex, &mut env))}
        Add(lh, rh) => { interp((*lh, &mut env)) + interp((*rh, &mut  env))}
    }
}

fn main() {
    println!("{:?}", Num(5));
    println!("{:?}", Add(&Num(5), &Num(6)));
    println!("{:?}", Add(&Read, &Negate(&Num(6))));
    println!("{:?}", interp((Add(&Read, &Negate(&Num(6))), &mut Env::new())));
    

}

#[cfg(test)]
mod tests {
    use super::*;

    fn a_interp(expr: Expr, expect:OType) {
        let prog = (expr, &mut Env::new());
        let res = interp(prog);
        assert_eq!(res, expect);
    }

    #[test]
    fn test_basic() {
        a_interp(Num(5), 5);
        a_interp(Num(-5), -5);
        a_interp(Add(&Num(5), &Num(6)), 11);
        a_interp(Add(&Read, &Read), 1);
        a_interp(Read, 0);
        a_interp(Negate(&Num(5)), -5);
        a_interp(Add(&Num(5), &Negate(&Num(6))), -1);
        a_interp(Add(&Read, &Negate(&Num(6))), -6);
        a_interp(Negate(&Negate(&Negate(&Num(6)))), -6);
        a_interp(Negate(&Negate(&Negate(&Num(0)))), 0);
    }
}
