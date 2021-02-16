use std::collections::HashMap;

trait Interp {
    fn interp(&self, env: &mut Env) -> Number;
}

type Variable = usize;
type Number = i64;
type Label = String;

#[derive(Clone)]
struct Env {
    read_count: usize,
    block_map: LabelMapping,
    var_map: HashMap<Variable, Number>,
}

impl Env {
    fn new(prog: &Program) -> Self {
        Env {
            read_count: 0,
            block_map: prog.clone(),
            var_map: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum Argument {
    Num(Number),
    Var(Variable),
}

#[derive(Debug, Clone)]
enum Expresion {
    Arg(Argument),
    Read(),
    Negate(Argument),
    Add(Argument, Argument),
}

#[derive(Debug, Clone)]
enum Statement {
    Set(Variable, Expresion),
}

#[derive(Debug, Clone)]
enum Tail {
    Return(Argument),
    Seq(Statement, Box<Tail>),
}

type LabelMapping = HashMap<Label, Tail>;
type Program = LabelMapping;

impl Interp for Label {
    fn interp(&self, env: &mut Env) -> Number {
        let target = env
            .clone()
            .block_map
            .get(self)
            .expect(format!("Program should have label {}, but does not", self).as_str())
            .clone();
        target.interp(env)
    }
}

impl Interp for Program {
    fn interp(&self, _: &mut Env) -> Number {
        let mut env = Env::new(self);
        let entry_point = Label::from("main");

        entry_point.interp(&mut env)
    }
}

impl Interp for Tail {
    fn interp(&self, env: &mut Env) -> Number {
        match self {
            Tail::Return(ret) => ret.interp(env),
            Tail::Seq(ex, rest) => {
                ex.interp(env);
                rest.interp(env)
            }
        }
    }
}

impl Interp for Statement {
    fn interp(&self, env: &mut Env) -> Number {
        match self {
            Statement::Set(v, ex) => {
                let val = ex.interp(env);
                env.var_map.insert(*v, val);
                val
            }
        }
    }
}

impl Interp for Argument {
    fn interp(&self, env: &mut Env) -> Number {
        match self {
            Argument::Num(n) => *n,
            Argument::Var(v) => *env.var_map.get(v).expect("Undefined variable"),
        }
    }
}

impl Interp for Expresion {
    fn interp(&self, env: &mut Env) -> Number {
        match self {
            Expresion::Arg(a) => a.interp(env),
            Expresion::Read() => {
                let count = env.read_count;
                env.read_count += 1;
                count as Number
            }
            Expresion::Negate(ex) => -1 * ex.interp(env),
            Expresion::Add(lh, rh) => lh.interp(env) + rh.interp(env),
        }
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
                                Box::new(Seq(Set(3, Negate(Var(2))), Box::new(Return(Var(3))))),
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
                                Box::new(Seq(Set(3, Negate(Var(2))), Box::new(Return(Var(3))))),
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
            let res = test_program.interp(&mut Env::new(&test_program));

            assert_eq!(
                res, expected_res,
                "\nExpression {:?},  returned an invalid result, {} != {}",
                test_program, res, expected_res
            )
        }
    }

    #[test]
    #[should_panic(expected = "Undefined variable")]
    fn test_c0_undefined_variable() {
        let test_progs: TestPrograms = vec![(
            vec![(
                Label::from("main"),
                Seq(Set(0, Arg(Num(5))), Box::new(Return(Var(1)))),
            )]
            .into_iter()
            .collect::<Program>(),
            5,
        )];

        for (test_program, expected_res) in test_progs {
            let res = test_program.interp(&mut Env::new(&test_program));

            assert_eq!(
                res, expected_res,
                "\nExpression {:?},  returned an invalid result, {} != {}",
                test_program, res, expected_res
            )
        }
    }
}