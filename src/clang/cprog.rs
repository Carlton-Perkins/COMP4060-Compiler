use std::collections::HashMap;

trait Interp {
    fn interp(&self, env: &mut Env) -> Number;
}

type Variable = usize;
type Number = i64;
type Label = String;

struct Env {}

impl Env {
    fn new() -> Self {
        Env {}
    }
}

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
    Seq(Statement, Box<Tail>),
}

type LabelMapping = HashMap<Label, Tail>;
type Program = LabelMapping;

impl Interp for Program {
    fn interp(&self, env: &mut Env) -> Number {
        0
    }
}

#[cfg(test)]
mod test_cprog {
    use super::*;
    use Argument::*;
    use Expresion::*;
    use Statement::*;
    use Tail::*;

    type Result = i64;
    type TestPrograms = Vec<(Program, Result)>;

    #[test]
    fn test_c0() {
        let test_progs: TestPrograms = vec![
            (
                vec![(
                    Label::from("main"),
                    Seq(Set(0, Arg(Num(5))), Box::new(Return(Var(0)))),
                )]
                .into_iter()
                .collect::<Program>(),
                5,
            ),
            (
                vec![(
                    Label::from("main"),
                    Seq(
                        Set(0, Arg(Num(5))),
                        Box::new(Seq(
                            Set(1, Arg(Num(6))),
                            Box::new(Seq(
                                Set(2, Add(Var(0), Var(1))),
                                Box::new(Seq(
                                    Set(3, Add(Var(0), Var(2))),
                                    Box::new(Return(Var(3))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<Program>(),
                16,
            ),
            (
                vec![(
                    Label::from("main"),
                    Seq(
                        Set(0, Arg(Num(5))),
                        Box::new(Seq(
                            Set(1, Arg(Num(6))),
                            Box::new(Seq(
                                Set(2, Add(Var(0), Var(1))),
                                Box::new(Seq(
                                    Set(3, Negate(Var(2))),
                                    Box::new(Return(Var(3))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<Program>(),
                -11,
            ),
            (
                vec![(
                    Label::from("main"),
                    Seq(
                        Set(0, Read()),
                        Box::new(Seq(
                            Set(1, Read()),
                            Box::new(Seq(
                                Set(2, Add(Var(0), Var(1))),
                                Box::new(Seq(
                                    Set(3, Negate(Var(2))),
                                    Box::new(Return(Var(3))),
                                )),
                            )),
                        )),
                    ),
                )]
                .into_iter()
                .collect::<Program>(),
                -1,
            ),
        ];

        for (test_program, expected_res) in test_progs {
            let res = test_program.interp(&mut Env::new());

            assert_eq!(
                res, expected_res,
                "\nExpression {:?},  returned an invalid result, {} != {}",
                test_program, res, expected_res
            )
        }
    }
}
