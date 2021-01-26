#[derive(Debug)]
enum Expr<'a> {
    Num(isize),
    Read,
    Negate(&'a Expr<'a>),
    Add(&'a Expr<'a>, &'a Expr<'a>)
}
#[derive(Debug)]
struct Program<'a> {
    e: Expr<'a>,
}


fn main() {
    println!("{:?}", Expr::Num(5));
    println!("{:?}", Expr::Add(&Expr::Num(5), &Expr::Num(6)));
}
