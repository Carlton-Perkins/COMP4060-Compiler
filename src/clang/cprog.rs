use std::collections::HashMap;

type Variable = usize;
type Number = i64;
type Label = String;

#[derive(Debug)]
enum Argument {
    Num(Number),
    Var(Variable),
}

#[derive(Debug)]
enum Expresion {
    Arg(Argument),
    Read(),
    Negate(Argument),
    Add(Argument, Argument),
}

#[derive(Debug)]
enum Statement {
    Set(Variable, Expresion),
}

#[derive(Debug)]
enum Tail {
    Return(Argument),
    More(Statement, Box<Tail>),
}

type LabelMapping = HashMap<Label, Tail>;
type Program = LabelMapping;
