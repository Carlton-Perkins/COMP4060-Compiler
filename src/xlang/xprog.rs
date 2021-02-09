use std::collections::HashMap;
use strum_macros;

trait Emit {
    fn emit(&self) -> String;
}

trait Interp {
    fn interp(&self, env: &Env) -> Env;
}

const NEWLINE: &str = "\n\r";

#[derive(Clone)]
struct Env {
    register: HashMap<Register, OType>,
    variable: HashMap<Var, OType>,
    memory: HashMap<Address, OType>,
    block: Program,
}

impl Env {
    fn new(prog: Program) -> Self {
        Env {
            register: HashMap::new(),
            variable: HashMap::new(),
            memory: HashMap::new(),
            block: prog,
        }
    }
}

type Var = usize;

type OType = i64;

type Label = String;
type Address = usize;

impl Emit for Label {
    fn emit(&self) -> String {
        self.to_string()
    }
}

impl Interp for Label {
    fn interp(&self, env: &Env) -> Env {
        let get = env.block.get(self).clone();
        match get {
            Some(blk) => blk.clone().interp(env),
            None => {
                panic!("No label {} found in program", self)
            }
        }
    }
}

#[derive(Debug, strum_macros::ToString, PartialEq, Eq, Hash, Clone, Copy)]
enum Register {
    RSP,
    RBP,
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Emit for Register {
    fn emit(&self) -> String {
        format!("%{}", self.to_string())
    }
}

#[derive(Clone, Copy, Debug)]
enum Argument {
    Con(OType),
    Reg(Register),
    Deref(Register, OType),
    Ref(Var),
}

impl Emit for Argument {
    fn emit(&self) -> String {
        match self {
            Argument::Con(n) => {
                format!("${}", n)
            }
            Argument::Reg(r) => r.emit(),
            Argument::Deref(r, n) => {
                format!("{}({})", r.emit(), n)
            }
            Argument::Ref(v) => {
                format!("{}", v)
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Instruction {
    Addq(Argument, Argument),
    Subq(Argument, Argument),
    Movq(Argument, Argument),
    Retq,
    Negq(Argument),
    Callq(Label),
    Jmp(Label),
    Pushq(Argument),
    Popq(Argument),
}

impl Emit for Instruction {
    fn emit(&self) -> String {
        fn binary(instr: &str, a1: &dyn Emit, a2: &dyn Emit) -> String {
            format!("{} {} {}", instr, a1.emit(), a2.emit())
        };
        fn unary(instr: &str, a: &dyn Emit) -> String {
            format!("{} {}", instr, a.emit())
        };

        match self {
            Instruction::Addq(src, dst) => binary("addq", src, dst),
            Instruction::Subq(src, dst) => binary("subq", src, dst),
            Instruction::Movq(src, dst) => binary("movq", src, dst),
            Instruction::Retq => "ret".to_string(),
            Instruction::Negq(n) => unary("negq", n),
            Instruction::Callq(dst) => unary("callq", dst),
            Instruction::Jmp(dst) => unary("jmp", dst),
            Instruction::Pushq(src) => unary("pushq", src),
            Instruction::Popq(dst) => unary("popq", dst),
        }
    }
}

fn set(dst: &Argument, val: &OType, env: &Env) -> Env {
    match dst {
        Argument::Con(con) => {
            panic!("Tried to set to a constant {} -> {:?}", con, dst)
        }
        Argument::Reg(reg) => {
            let mut env_c = env.clone();
            env_c.register.insert(*reg, *val);
            env_c
        }
        Argument::Deref(reg, offset) => {
            let mut env_c = env.clone();
            let reg_val = env_c.register.get(reg).unwrap_or(&0); // Default registers to 0
            let target = reg_val + offset;
            env_c.memory.insert(target as usize, *val);
            env_c
        }
        Argument::Ref(var) => {
            let mut env_c = env.clone();
            env_c.memory.insert(*var, *val);
            env_c
        }
    }
}

fn value(arg: &Argument, env: &Env) -> OType {
    match arg {
        Argument::Con(c) => *c,
        Argument::Reg(reg) => *env.register.get(reg).unwrap_or(&0), // Default registers to 0
        Argument::Deref(reg, offset) => {
            let reg_val = env.register.get(reg).unwrap_or(&0); // Default registers to 0
            let target = reg_val + offset;
            *env.memory.get(&(target as usize)).unwrap_or(&0) // Defaults memory to 0
        }
        Argument::Ref(var) => {
            *env.variable.get(var).unwrap_or(&0) // Default variables to 0
        }
    }
}

fn push(src: &Argument, env: &Env) -> Env {
    let mut env_c = env.clone();
    let val = value(src, &env_c);
    env_c = set(
        &Argument::Reg(Register::RSP),
        &(value(&Argument::Deref(Register::RSP, 0), &env_c) - 8),
        &env_c,
    );
    set(&Argument::Deref(Register::RSP, 0), &val, &env_c)
}

fn pop(dst: &Argument, env: &Env) -> Env {
    let mut env_c = env.clone();
    let val = value(&Argument::Deref(Register::RSP, 0), &env_c);
    env_c = set(dst, &val, &env_c);
    set(
        &Argument::Reg(Register::RSP),
        &(value(&Argument::Reg(Register::RSP), &env_c) + 8),
        &env_c,
    )
}

impl Interp for Instruction {
    fn interp(&self, env: &Env) -> Env {
        match self {
            Instruction::Addq(src, dst) => set(dst, &(value(src, env) + value(dst, env)), env),
            Instruction::Subq(src, dst) => set(dst, &(value(src, env) - value(dst, env)), env),
            Instruction::Movq(src, dst) => set(dst, &value(src, env), env),
            Instruction::Retq => env.clone(),
            Instruction::Negq(v) => set(v, &value(v, env), env),
            Instruction::Callq(l) => l.interp(env),
            Instruction::Jmp(l) => l.interp(env),
            Instruction::Pushq(src) => push(src, env),
            Instruction::Popq(dst) => pop(dst, env),
        }
    }
}

type Block = Vec<Instruction>;

impl Emit for Block {
    fn emit(&self) -> String {
        self.into_iter()
            .map(|x| x.emit() + NEWLINE)
            .fold("".to_string(), |acc, s| acc + &s)
    }
}

impl Interp for Block {
    fn interp(&self, env: &Env) -> Env {
        match self.split_first() {
            Some((first, rest)) => rest
                .into_iter()
                .map(|x| x.clone())
                .collect::<Block>()
                .interp(&first.interp(&env)),
            None => env.clone(),
        }
    }
}

type Program = HashMap<Label, Block>;

impl Emit for Program {
    fn emit(&self) -> String {
        self.into_iter()
            .map(|(lab, blk)| format!("{0}:{1}{2}{1}{1}", lab.emit(), NEWLINE, blk.emit()))
            .fold("".to_string(), |acc, s| acc + &s)
    }
}

impl Interp for Program {
    fn interp(&self, _: &Env) -> Env {
        let env = Env::new(self.clone());
        Label::from("_main").interp(&env)
    }
}

#[cfg(test)]
mod test_xprog {
    use std::vec;

    use super::Argument::*;
    use super::Instruction::*;
    use super::Register::*;
    use super::*;

    fn compile_and_run(_prog: &Program) -> Result<i64, String> {
        unimplemented!();
    }

    #[test]
    fn test_emit() {
        assert_eq!(Register::R10.emit(), "%R10");
        assert_eq!(Argument::Con(5).emit(), "$5");
        assert_eq!(Argument::Reg(Register::RAX).emit(), "%RAX");
        assert_eq!(Argument::Deref(Register::RAX, 5).emit(), "%RAX(5)");
    }

    fn get_test_progs() -> Vec<(Program, i64)> {
        let test_progs: Vec<(Program, i64)> = vec![
            (
                vec![(
                    "_main".to_string(),
                    vec![Movq(Con(5), Reg(RAX)), Movq(Con(6), Reg(R9)), Retq],
                )]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                5,
            ),
            (
                vec![(
                    "_main".to_string(),
                    vec![
                        Movq(Con(5), Reg(RAX)),
                        Movq(Con(6), Reg(R9)),
                        Addq(Reg(R9), Reg(RAX)),
                        Retq,
                    ],
                )]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                11,
            ),
            (
                vec![
                    ("foo".to_string(), vec![Movq(Con(33), Reg(RAX)), Retq]),
                    ("_main".to_string(), vec![Jmp("foo".to_string()), Retq]),
                ]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                33,
            ),
            (
                vec![(
                    "_main".to_string(),
                    vec![
                        Movq(Con(5), Reg(RAX)),
                        Movq(Con(6), Reg(R9)),
                        Pushq(Reg(R9)),
                        Popq(Reg(RAX)),
                        Retq,
                    ],
                )]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                6,
            ),
        ];

        test_progs
    }

    #[test]
    fn test_xprog() {
        for (test_prog, expected_res) in get_test_progs() {
            let res = compile_and_run(&test_prog);
            let interp_env = Env::new(HashMap::new());
            let res_env = test_prog.interp(&interp_env);

            assert_eq!(res_env.register[&RAX], expected_res);

            match res {
                Ok(n) => {
                    assert_eq!(
                        n,
                        expected_res,
                        "Program returned {} when it should have returned {}, Program: {}{}",
                        n,
                        expected_res,
                        NEWLINE,
                        test_prog.emit()
                    );
                }
                Err(error) => {
                    assert!(
                        false,
                        format!(
                            "Compile failed with error {}, {}{}",
                            error,
                            NEWLINE,
                            test_prog.emit()
                        )
                    );
                }
            }
        }
    }

    #[test]
    fn test_xprog_interp() {
        for (test_prog, expected_res) in get_test_progs() {
            let interp_env = Env::new(HashMap::new());
            let res_env = test_prog.interp(&interp_env);

            assert_eq!(
                value(&Reg(RAX), &res_env),
                expected_res,
                "Program returned {}, when it should have returned {}: {:?}",
                value(&Reg(RAX), &res_env),
                expected_res,
                test_prog
            );
        }
    }
}
