use crate::common::types::{Address, Label, Number};
use crate::common::{Emit, Interp};
use std::collections::HashMap;
use strum_macros;

const NEWLINE: &str = "\n";

type Variable = usize;
type Block = Vec<Instruction>;
type XProgram = HashMap<Label, Block>;

#[derive(Clone)]
pub struct XEnv {
    register: HashMap<Register, Number>,
    variable: HashMap<Variable, Number>,
    memory: HashMap<Address, Number>,
    block: XProgram,
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

#[derive(Clone, Debug)]
enum Argument {
    Con(Number),
    Reg(Register),
    Deref(Register, Number),
    Ref(Variable),
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

impl XEnv {
    fn new(prog: XProgram) -> Self {
        XEnv {
            register: HashMap::new(),
            variable: HashMap::new(),
            memory: HashMap::new(),
            block: prog,
        }
    }
}

impl Emit for Register {
    fn emit(&self) -> String {
        format!("%{}", self.to_string())
    }
}

impl Emit for Label {
    fn emit(&self) -> String {
        self.to_string()
    }
}

impl Emit for Argument {
    fn emit(&self) -> String {
        match self {
            Argument::Con(n) => format!("${}", n),
            Argument::Reg(r) => r.emit(),
            Argument::Deref(r, n) => format!("{}({})", r.emit(), n),
            Argument::Ref(v) => format!("{}", v),
        }
    }
}

impl Emit for Instruction {
    fn emit(&self) -> String {
        fn binary(instr: &str, a1: &dyn Emit, a2: &dyn Emit) -> String {
            format!("{} {}, {}", instr, a1.emit(), a2.emit())
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

impl Emit for Block {
    fn emit(&self) -> String {
        self.into_iter()
            .map(|x| x.emit() + NEWLINE)
            .fold("".to_string(), |acc, s| acc + &s)
    }
}

impl Emit for XProgram {
    fn emit(&self) -> String {
        String::new()
            + ".globl main\n"
            + self
                .into_iter()
                .map(|(lab, blk)| format!("{0}:{1}{2}{1}{1}", lab.emit(), NEWLINE, blk.emit()))
                .fold("".to_string(), |acc, s| acc + s.as_str())
                .as_str()
    }
}

impl Interp for Label {
    type Env = XEnv;
    type Output = XEnv;

    fn interp(&self, env: &Self::Env) -> Self::Output {
        let get = env.block.get(self).clone();
        match get {
            Some(blk) => blk.clone().interp(env),
            None => panic!("No label {} found in program", self),
        }
    }
}

impl Interp for Instruction {
    type Env = XEnv;
    type Output = XEnv;

    fn interp(&self, env: &Self::Env) -> Self::Output {
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

impl Interp for Block {
    type Env = XEnv;
    type Output = XEnv;

    fn interp(&self, env: &Self::Env) -> Self::Output {
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

impl Interp for XProgram {
    type Env = XEnv;
    type Output = XEnv;

    fn interp(&self, _: &Self::Env) -> Self::Output {
        let env = Self::Env::new(self.clone());
        Label::from("main").interp(&env)
    }
}

fn set(dst: &Argument, val: &Number, env: &XEnv) -> XEnv {
    match dst {
        Argument::Con(con) => panic!("Tried to set to a constant {} -> {:?}", con, dst),
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

fn value(arg: &Argument, env: &XEnv) -> Number {
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

fn push(src: &Argument, env: &XEnv) -> XEnv {
    let mut env_c = env.clone();
    let val = value(src, &env_c);
    env_c = set(
        &Argument::Reg(Register::RSP),
        &(value(&Argument::Deref(Register::RSP, 0), &env_c) - 8),
        &env_c,
    );
    set(&Argument::Deref(Register::RSP, 0), &val, &env_c)
}

fn pop(dst: &Argument, env: &XEnv) -> XEnv {
    let mut env_c = env.clone();
    let val = value(&Argument::Deref(Register::RSP, 0), &env_c);
    env_c = set(dst, &val, &env_c);
    set(
        &Argument::Reg(Register::RSP),
        &(value(&Argument::Reg(Register::RSP), &env_c) + 8),
        &env_c,
    )
}

#[cfg(test)]
mod test_xprog {
    use std::{
        fs::File,
        io::{stderr, stdout, Write},
        os::unix::process::ExitStatusExt,
        process::{Command, Output},
        vec,
    };

    use tempfile::tempdir;

    use super::Argument::*;
    use super::Instruction::*;
    use super::Register::*;
    use super::*;

    fn compile_and_run(prog: &String) -> Result<i32, String> {
        // Create new file with arg string
        let dir = tempdir().expect("Failled to alocate temp dir");
        println!("TempDir: {:?}", dir.as_ref());
        let temp_file = dir.path().join("asm.s");
        let mut file = File::create(temp_file.clone()).expect("Failed to create temp file");
        file.write_all(prog.as_bytes())
            .expect("Failed to write asm to file");
        // Assemble that file
        let asmer = "gcc";
        let temp_file_path = temp_file.as_path().to_str().unwrap();
        let asm_args = &["-c", temp_file_path, "-g"];
        let asm_res =
            run_cmd_and_print(Command::new(asmer).current_dir(dir.as_ref()).args(asm_args));

        match asm_res.status.code().unwrap() {
            0 => println!("ASM Success"),
            n => {
                println!("ASM Failed with code {}", n);
                stdout().write_all(&asm_res.stdout).unwrap();
                stderr().write_all(&asm_res.stderr).unwrap();
                return Err(format!("ASM Failed with code {}", n));
            }
        }

        // Link that file
        let linker = "gcc";
        let linker_args = &["-o", "asm", "asm.o", "-g"];
        let linker_res = run_cmd_and_print(
            Command::new(linker)
                .current_dir(dir.as_ref())
                .args(linker_args),
        );

        match linker_res.status.code().unwrap() {
            0 => println!("Link Success"),
            n => {
                println!("Link Failed with code {}", n);
                stdout().write_all(&linker_res.stdout).unwrap();
                stderr().write_all(&linker_res.stderr).unwrap();
                return Err(format!("Link Failed with code {}", n));
            }
        }

        // Run the resulting binary
        let run_res = run_cmd_and_print(Command::new("./asm").current_dir(dir.as_ref()));

        match run_res.status.code() {
            Some(n) => Ok(n),
            None => match run_res.status.signal() {
                Some(sign) => Err(format!("Run Failure with sig {}", sign)),
                None => Err("Run Unknown Failure".to_string()),
            },
        }
    }
    fn run_cmd_and_print(c: &mut Command) -> Output {
        c.output().expect(&format!("CMD {:?} failed", c))
    }

    #[test]
    fn test_emit() {
        assert_eq!(Register::R10.emit(), "%R10");
        assert_eq!(Argument::Con(5).emit(), "$5");
        assert_eq!(Argument::Reg(Register::RAX).emit(), "%RAX");
        assert_eq!(Argument::Deref(Register::RAX, 5).emit(), "%RAX(5)");
    }

    fn get_test_progs() -> Vec<(XProgram, i64)> {
        let test_progs: Vec<(XProgram, i64)> = vec![
            (
                vec![(
                    "main".to_string(),
                    vec![Movq(Con(5), Reg(RAX)), Movq(Con(6), Reg(R9)), Retq],
                )]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                5,
            ),
            (
                vec![(
                    "main".to_string(),
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
                    ("main".to_string(), vec![Jmp("foo".to_string()), Retq]),
                ]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                33,
            ),
            (
                vec![(
                    "main".to_string(),
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
            let res = compile_and_run(&test_prog.emit());
            let interp_env = XEnv::new(HashMap::new());
            let res_env = test_prog.interp(&interp_env);

            assert_eq!(res_env.register[&RAX], expected_res);

            match res {
                Ok(n) => {
                    assert_eq!(
                        n as i64,
                        expected_res,
                        "Program returned {} when it should have returned {}, Program: {}{}",
                        n as i64,
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
            let interp_env = XEnv::new(HashMap::new());
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
