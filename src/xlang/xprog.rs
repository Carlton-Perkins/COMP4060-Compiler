use crate::common::{
    traits::Emit,
    types::{Address, Answer, Answer::*, Label, Number, Variable, CMP},
};
use itertools::Itertools;
use std::collections::HashMap;
use strum_macros;

const NEWLINE: &str = "\n";

pub type XBlock = Vec<XInstruction>;
pub type XProgram = HashMap<Label, XBlock>;

pub trait XInterpMut {
    type Env;
    type Output;

    fn interp(&self) -> Self::Output;
    fn interp_(&self, env: &mut Self::Env) -> Self::Output;
}

trait ToByteReg {
    fn to_byte_register(&self) -> XByteRegister;
}

#[derive(Clone)]
pub struct XEnv {
    register: HashMap<XRegister, Number>,
    variable: HashMap<Variable, Number>,
    memory: HashMap<Address, Number>,
    program: XProgram,
    current_block: XBlock,
    readc: usize,
    printed: Vec<Number>,
    cmp: XCMPEnv,
}
#[derive(Clone, Copy)]
struct XCMPEnv {
    eq: bool,
    lt: bool,
    leq: bool,
    geq: bool,
    gt: bool,
}

#[derive(Debug, strum_macros::ToString, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub enum XRegister {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Debug, strum_macros::ToString, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub enum XByteRegister {
    AL,
    BL,
    CL,
    DL,
    SIL,
    DIL,
    BPL,
    SPL,
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,
}
mod reglist {
    use super::{XRegister, XRegister::*};

    pub const ALL_REGISTERS: &[XRegister] = &[
        RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15,
    ];

    pub const CALLER_SAVED_REGISTERS: &[XRegister] = &[RAX, RDX, RCX, RSI, RDI, R8, R9, R10, R11];

    pub const CALLEE_SAVED_REGISTERS: &[XRegister] = &[RBX, RBP, R12, R13, R14, R15];

    pub const USEABLE_REGISTERS: &[XRegister] = &[
        RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15,
    ];

    pub const TEMP_REGISTER: XRegister = RAX;
}
pub use reglist::{
    ALL_REGISTERS, CALLEE_SAVED_REGISTERS, CALLER_SAVED_REGISTERS, TEMP_REGISTER, USEABLE_REGISTERS,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum XArgument {
    XCon(Number),
    XReg(XRegister),
    XBReg(XRegister),
    XDeref(XRegister, Number),
    XVar(Variable),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum XInstruction {
    Addq(XArgument, XArgument),
    Subq(XArgument, XArgument),
    Movq(XArgument, XArgument),
    Retq,
    Negq(XArgument),
    Callq(Label),
    Jmp(Label),
    Pushq(XArgument),
    Popq(XArgument),
    Xorq(XArgument, XArgument),
    Cmpq(XArgument, XArgument),
    Set(CMP, XArgument),
    Movzbq(XArgument, XArgument),
    JmpIf(CMP, Label),
}

impl ToByteReg for XRegister {
    fn to_byte_register(&self) -> XByteRegister {
        use {XByteRegister::*, XRegister::*};
        match self {
            RAX => AL,
            RBX => BL,
            RCX => CL,
            RDX => DL,
            RSI => SIL,
            RDI => DIL,
            RBP => BPL,
            RSP => SPL,
            R8 => R8B,
            R9 => R9B,
            R10 => R10B,
            R11 => R11B,
            R12 => R12B,
            R13 => R13B,
            R14 => R14B,
            R15 => R15B,
        }
    }
}

impl ToByteReg for XArgument {
    fn to_byte_register(&self) -> XByteRegister {
        match self {
            XArgument::XCon(_) => {
                panic!("Impossible conversion self -> ByteRegister")
            }
            XArgument::XReg(r) => r.to_byte_register(),
            XArgument::XBReg(br) => br.to_byte_register(),
            XArgument::XDeref(_, _) => {
                panic!("Impossible conversion self -> ByteRegister")
            }
            XArgument::XVar(_) => {
                panic!("Impossible conversion self -> ByteRegister")
            }
        }
    }
}

impl XEnv {
    pub fn new(prog: &XProgram) -> Self {
        XEnv {
            register: HashMap::new(),
            variable: HashMap::new(),
            memory: HashMap::new(),
            program: prog.clone(),
            current_block: vec![],
            readc: 0,
            printed: Vec::new(),
            cmp: XCMPEnv::new(),
        }
    }
}

impl XCMPEnv {
    fn new() -> Self {
        XCMPEnv {
            eq: false,
            lt: false,
            leq: false,
            geq: false,
            gt: false,
        }
    }

    fn get(&self, cmp: &CMP) -> bool {
        match cmp {
            CMP::EQ => self.eq,
            CMP::LT => self.lt,
            CMP::LEQ => self.leq,
            CMP::GEQ => self.geq,
            CMP::GT => self.gt,
        }
    }

    fn do_cmp(lh: &Number, rh: &Number) -> XCMPEnv {
        XCMPEnv {
            eq: lh == rh,
            lt: lh < rh,
            leq: lh <= rh,
            geq: lh >= rh,
            gt: lh > rh,
        }
    }
}

impl Emit for CMP {
    fn emit(&self) -> String {
        match self {
            CMP::EQ => "e",
            CMP::LT => "l",
            CMP::LEQ => "le",
            CMP::GEQ => "g",
            CMP::GT => "ge",
        }
        .into()
    }
}

impl Emit for XRegister {
    fn emit(&self) -> String {
        format!("%{}", self.to_string())
    }
}

impl Emit for XByteRegister {
    fn emit(&self) -> String {
        format!("%{}", self.to_string())
    }
}

impl Emit for Label {
    fn emit(&self) -> String {
        self.to_string()
    }
}

impl Emit for XArgument {
    fn emit(&self) -> String {
        match self {
            XArgument::XCon(n) => format!("${}", n),
            XArgument::XReg(r) => r.emit(),
            XArgument::XDeref(r, n) => match n {
                0 => format!("({})", r.emit()),
                n => format!("{}({})", n, r.emit()),
            },
            XArgument::XVar(v) => format!("!{}!", v),
            XArgument::XBReg(br) => br.to_byte_register().emit(),
        }
    }
}

impl Emit for XInstruction {
    fn emit(&self) -> String {
        fn binary(instr: &str, a1: &dyn Emit, a2: &dyn Emit) -> String {
            format!("{} {}, {}", instr, a1.emit(), a2.emit())
        };
        fn unary(instr: &str, a: &dyn Emit) -> String {
            format!("{} {}", instr, a.emit())
        };

        match self {
            XInstruction::Addq(src, dst) => binary("addq", src, dst),
            XInstruction::Subq(src, dst) => binary("subq", src, dst),
            XInstruction::Movq(src, dst) => binary("movq", src, dst),
            XInstruction::Retq => "retq".to_string(),
            XInstruction::Negq(n) => unary("negq", n),
            XInstruction::Callq(dst) => unary("callq", dst),
            XInstruction::Jmp(dst) => unary("jmp", dst),
            XInstruction::Pushq(src) => unary("pushq", src),
            XInstruction::Popq(dst) => unary("popq", dst),
            XInstruction::Xorq(src, dst) => binary("xorq", src, dst),
            XInstruction::Cmpq(src, dst) => binary("cmpq", src, dst),
            XInstruction::Set(cmp, dst) => unary(
                &(String::from("set") + &cmp.emit()),
                &dst.to_byte_register(),
            ),
            XInstruction::Movzbq(src, dst) => binary("movzbq", src, dst),
            XInstruction::JmpIf(cmp, lab) => unary(&(String::from("j") + &cmp.emit()), lab),
        }
    }
}

impl Emit for XBlock {
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

impl XInterpMut for Label {
    type Env = XEnv;
    type Output = XEnv;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        let get = env.program.get(self).clone();
        match get {
            Some(blk) => blk.clone().interp_(env),
            None => match self.as_str() {
                "_read_int" => {
                    let blk = vec![XInstruction::Movq(
                        XArgument::XCon(env.readc as Number),
                        XArgument::XReg(XRegister::RAX),
                    )];
                    env.readc += 1;
                    blk.interp_(env)
                }
                "_print_int" => {
                    env.printed
                        .push(*env.register.get(&XRegister::RDI).unwrap_or(&0));
                    env.clone()
                }
                _ => panic!("No label {} found in program", self),
            },
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

impl XInterpMut for XInstruction {
    type Env = XEnv;
    type Output = XEnv;

    fn interp_(&self, env: &mut Self::Env) -> Self::Output {
        println!("Running: {:?}", self);
        match self {
            XInstruction::Addq(src, dst) => set(dst, &(value(src, env) + value(dst, env)), env),
            XInstruction::Subq(src, dst) => set(dst, &(value(src, env) - value(dst, env)), env),
            XInstruction::Movq(src, dst) => set(dst, &value(src, env), env),
            XInstruction::Retq => env.clone(),
            XInstruction::Negq(v) => set(v, &(-1 * value(v, env)), env),
            XInstruction::Callq(l) => l.interp_(env),
            XInstruction::Jmp(l) => l.interp_(env),
            XInstruction::Pushq(src) => push(src, env),
            XInstruction::Popq(dst) => pop(dst, env),
            XInstruction::Xorq(src, dst) => set(dst, &(value(dst, env) ^ value(src, env)), env),
            XInstruction::Cmpq(lh, rh) => {
                let mut new_env = env.clone();
                new_env.cmp = XCMPEnv::do_cmp(&value(lh, env), &value(rh, env));
                new_env
            }
            XInstruction::Set(cmp, dst) => {
                let c = if env.cmp.get(cmp) { 1 } else { 0 };
                set(dst, &c, env)
            }
            XInstruction::Movzbq(src, dst) => set(dst, &(value(src, env) & 0xFF), env),
            XInstruction::JmpIf(cmp, lab) => {
                if env.cmp.get(cmp) {
                    lab.interp_(env)
                } else {
                    env.clone()
                }
            }
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

impl XInterpMut for XBlock {
    type Env = XEnv;
    type Output = XEnv;

    fn interp_(&self, mut env: &mut Self::Env) -> Self::Output {
        let mut new_env = env.clone();
        new_env.current_block = self.clone();
        loop {
            match new_env.clone().current_block.split_first() {
                Some((first, rest)) => {
                    new_env.current_block = rest.to_vec();
                    new_env = first.interp_(&mut new_env);
                }
                None => return new_env.clone(),
            }
        }
    }

    fn interp(&self) -> Self::Output {
        unimplemented!()
    }
}

impl XInterpMut for XProgram {
    type Env = XEnv;
    type Output = Answer;

    fn interp_(&self, mut env: &mut Self::Env) -> Self::Output {
        let ret_env = Label!("main").interp_(&mut env);
        S64(*ret_env
            .printed
            .get(0)
            .unwrap_or(&value(&XArgument::XReg(XRegister::RAX), &ret_env)))
    }

    fn interp(&self) -> Self::Output {
        self.interp_(&mut XEnv::new(&self.clone()))
    }
}

fn set(dst: &XArgument, val: &Number, env: &XEnv) -> XEnv {
    match dst {
        XArgument::XCon(con) => panic!("Tried to set to a constant {} -> {:?}", con, dst),
        XArgument::XReg(reg) => {
            let mut env_c = env.clone();
            env_c.register.insert(*reg, *val);
            env_c
        }
        XArgument::XDeref(reg, offset) => {
            let mut env_c = env.clone();
            let reg_val = env_c.register.get(reg).unwrap_or(&0); // Default registers to 0
            let target = reg_val + offset;
            env_c.memory.insert(target as usize, *val);
            env_c
        }
        XArgument::XVar(var) => {
            let mut env_c = env.clone();
            env_c.variable.insert(var.into(), *val);
            env_c
        }
        XArgument::XBReg(r) => {
            let mut env_c = env.clone();
            env_c.register.insert(*r, val & 0xFF);
            env_c
        }
    }
}

fn value(arg: &XArgument, env: &XEnv) -> Number {
    match arg {
        XArgument::XCon(c) => *c,
        XArgument::XReg(reg) => *env.register.get(reg).unwrap_or(&0), // Default registers to 0
        XArgument::XDeref(reg, offset) => {
            let reg_val = env.register.get(reg).unwrap_or(&0); // Default registers to 0
            let target = reg_val + offset;
            *env.memory.get(&(target as usize)).unwrap_or(&0) // Defaults memory to 0
        }
        XArgument::XVar(var) => {
            *env.variable.get(var).unwrap_or(&0) as Number // Default variables to 0
        }
        XArgument::XBReg(r) => *env.register.get(r).unwrap_or(&0) & 0xFF,
    }
}

fn push(src: &XArgument, env: &XEnv) -> XEnv {
    let mut env_c = env.clone();
    let val = value(src, &env_c);
    env_c = set(
        &XArgument::XReg(XRegister::RSP),
        &(value(&XArgument::XDeref(XRegister::RSP, 0), &env_c) - 8),
        &env_c,
    );
    set(&XArgument::XDeref(XRegister::RSP, 0), &val, &env_c)
}

fn pop(dst: &XArgument, env: &XEnv) -> XEnv {
    let mut env_c = env.clone();
    let val = value(&XArgument::XDeref(XRegister::RSP, 0), &env_c);
    env_c = set(dst, &val, &env_c);
    set(
        &XArgument::XReg(XRegister::RSP),
        &(value(&XArgument::XReg(XRegister::RSP), &env_c) + 8),
        &env_c,
    )
}

#[cfg(test)]
mod test_xprog {
    use super::XArgument::*;
    use super::XInstruction::*;
    use super::XRegister::*;
    use super::*;
    use crate::xlang::compile_and_run;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_emit() {
        assert_eq!(XRegister::R10.emit(), "%R10");
        assert_eq!(XArgument::XCon(5).emit(), "$5");
        assert_eq!(XArgument::XReg(XRegister::RAX).emit(), "%RAX");
        assert_eq!(XArgument::XDeref(XRegister::RAX, 5).emit(), "5(%RAX)");
        assert_eq!(XArgument::XDeref(XRegister::RAX, 0).emit(), "(%RAX)");
        assert_eq!(XArgument::XBReg(XRegister::RAX).emit(), "%AL");
        assert_eq!(
            XInstruction::Set(CMP::EQ, XArgument::XBReg(XRegister::RAX)).emit(),
            "sete %AL"
        );
        assert_eq!(XInstruction::JmpIf(CMP::EQ, Label!("foo")).emit(), "je foo");
    }

    fn get_test_progs() -> Vec<(XProgram, Answer)> {
        let test_progs: Vec<(XProgram, Answer)> = vec![
            (
                vec![(
                    "main".to_string(),
                    vec![Movq(XCon(5), XReg(RAX)), Movq(XCon(6), XReg(R9)), Retq],
                )]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                S64(5),
            ),
            (
                vec![(
                    "main".to_string(),
                    vec![
                        Movq(XCon(5), XReg(RAX)),
                        Movq(XCon(6), XReg(R9)),
                        Addq(XReg(R9), XReg(RAX)),
                        Retq,
                    ],
                )]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                S64(11),
            ),
            (
                vec![
                    ("foo".to_string(), vec![Movq(XCon(33), XReg(RAX)), Retq]),
                    ("main".to_string(), vec![Jmp("foo".to_string()), Retq]),
                ]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                S64(33),
            ),
            (
                vec![(
                    "main".to_string(),
                    vec![
                        Movq(XCon(5), XReg(RAX)),
                        Movq(XCon(6), XReg(R9)),
                        Pushq(XReg(R9)),
                        Popq(XReg(RAX)),
                        Retq,
                    ],
                )]
                .into_iter()
                .collect::<HashMap<_, _>>(),
                S64(6),
            ),
            (
                XProgram!(XBlock!(
                    "main",
                    Movq(XCon(1), XReg(RAX)),
                    Movq(XCon(1), XReg(RBX)),
                    Xorq(XReg(RBX), XReg(RAX)),
                    Retq
                )),
                S64(0),
            ),
            (
                XProgram!(XBlock!(
                    "main",
                    Movq(XCon(1), XReg(RAX)),
                    Movq(XCon(5), XReg(RBX)),
                    Cmpq(XReg(RAX), XReg(RBX)),
                    Set(CMP::EQ, XReg(RAX)),
                    Retq
                )),
                // Bool(false), TODO Should really contain type info
                S64(0),
            ),
            (
                XProgram!(XBlock!(
                    "main",
                    Movq(XCon(1), XReg(RAX)),
                    Movq(XCon(1), XReg(RBX)),
                    Cmpq(XReg(RAX), XReg(RBX)),
                    Set(CMP::EQ, XReg(RAX)),
                    Retq
                )),
                // Bool(true), TODO Should really contain type info
                S64(1),
            ),
            (
                XProgram!(
                    XBlock!(
                        "main",
                        Movq(XCon(1), XReg(RAX)),
                        Movq(XCon(1), XReg(RBX)),
                        Cmpq(XReg(RAX), XReg(RBX)),
                        JmpIf(CMP::EQ, Label!("tbranch")),
                        Jmp(Label!("fbranch"))
                    ),
                    XBlock!(
                        "tbranch",
                        Movq(XCon(24), XReg(RAX)),
                        Jmp(Label!("endbranch"))
                    ),
                    XBlock!(
                        "fbranch",
                        Movq(XCon(42), XReg(RAX)),
                        Jmp(Label!("endbranch"))
                    ),
                    XBlock!("endbranch", Retq)
                ),
                S64(24),
            ),
            (
                XProgram!(
                    XBlock!(
                        "main",
                        Movq(XCon(2), XReg(RAX)),
                        Movq(XCon(1), XReg(RBX)),
                        Cmpq(XReg(RAX), XReg(RBX)),
                        JmpIf(CMP::EQ, Label!("tbranch")),
                        Jmp(Label!("fbranch"))
                    ),
                    XBlock!(
                        "tbranch",
                        Movq(XCon(24), XReg(RAX)),
                        Jmp(Label!("endbranch"))
                    ),
                    XBlock!(
                        "fbranch",
                        Movq(XCon(42), XReg(RAX)),
                        Jmp(Label!("endbranch"))
                    ),
                    XBlock!("endbranch", Retq)
                ),
                S64(42),
            ),
            (
                XProgram!(
                    XBlock!(
                        "main",
                        Movq(XCon(2), XReg(RAX)),
                        Movq(XCon(1), XReg(RBX)),
                        Jmp(Label!("fbranch"))
                    ),
                    XBlock!("tbranch", Callq(Label!("foo")), Jmp(Label!("endbranch"))),
                    XBlock!(
                        "fbranch",
                        Movq(XCon(42), XReg(RAX)),
                        Jmp(Label!("endbranch"))
                    ),
                    XBlock!("foo", Movq(XCon(4444), XReg(RAX)), Retq),
                    XBlock!("endbranch", Retq)
                ),
                S64(42),
            ),
            (
                XProgram!(
                    XBlock!(
                        "main",
                        Movq(XCon(2), XReg(RAX)),
                        Movq(XCon(1), XReg(RBX)),
                        Jmp(Label!("tbranch"))
                    ),
                    XBlock!("tbranch", Callq(Label!("foo")), Jmp(Label!("endbranch"))),
                    XBlock!(
                        "fbranch",
                        Movq(XCon(42), XReg(RAX)),
                        Jmp(Label!("endbranch"))
                    ),
                    XBlock!("foo", Movq(XCon(44), XReg(RAX)), Retq),
                    XBlock!("endbranch", Retq)
                ),
                S64(44),
            ),
        ];

        test_progs
    }

    #[test]
    fn test_xprog() {
        for (test_prog, expected_res) in get_test_progs() {
            let c_res = compile_and_run(&test_prog.emit());
            let i_res = test_prog.interp();

            assert_eq!(i_res, expected_res);

            match c_res {
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
            let res = test_prog.interp();

            assert_eq!(
                res, expected_res,
                "Program returned {}, when it should have returned {}: {:?}",
                res, expected_res, test_prog
            );
        }
    }
}
