#[derive(Debug, PartialEq)]
enum Expr<'a> {
    Num(isize),
    Read,
    Negate(&'a Expr<'a>),
    Add(&'a Expr<'a>, &'a Expr<'a>),
}
use Expr::*;

use Expr as Program;

fn interp(prog: Program) -> isize {
    0
}

fn main() {
    println!("{:?}", Num(5));
    println!("{:?}", Add(&Num(5), &Num(6)));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        assert_eq!(interp(Num(5)), 5);
        assert_eq!(interp(Num(-5)), -5);
        assert_eq!(interp(Add(&Num(5), &Num(6))), 11);
        assert_eq!(interp(Read), 0);
        assert_eq!(interp(Negate(&Num(5))), -5);
        assert_eq!(interp(Add(&Num(5), &Negate(&Num(6)))), -1);
        assert_eq!(interp(Add(&Read, &Negate(&Num(6)))), -6);
        assert_eq!(interp(Negate(&Negate(&Negate(&Num(6))))), -6);
        assert_eq!(interp(Negate(&Negate(&Negate(&Num(0))))), 0);
    }
}
