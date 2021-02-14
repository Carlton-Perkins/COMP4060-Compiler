use std::collections::HashMap;

type Variable = usize;
type Number = i64;
type Label = String;

enum Argument {
    Num(Number),
    Var(Variable),
}

enum Expresion {
    Arg(Argument),
    Read(),
    Negate(Argument),
    Add(Argument, Argument),
}

enum Statement {
    Set(Variable, Expresion),
}

enum Tail {
    Return(Argument),
    More(Statement, Box<Tail>),
}

type LabelMapping = HashMap<Label, Tail>;
type Program = LabelMapping;
