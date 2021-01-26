enum Expr<'a> {
    Num(isize),
    Read,
    Negate(&'a Expr<'a>),
    Add(&'a Expr<'a>, &'a Expr<'a>)
}

struct Program<'a> {
    e: Expr<'a>,
}


fn main() {
    println!("Hello, world!");
}
