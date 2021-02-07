use std::collections::HashMap;
use strum_macros;

trait Emit {
    fn emit(&self) -> String;
}

const NEWLINE: &str = "\n\r";

type Var = usize;

type OType = i64;

type Label = String;

impl Emit for Label {
    fn emit(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, strum_macros::ToString)]
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

type Block = Vec<Instruction>;

impl Emit for Block {
    fn emit(&self) -> String {
        self.into_iter()
            .map(|x| x.emit() + NEWLINE)
            .fold("".to_string(), |acc, s| acc + &s)
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

#[cfg(test)]
mod test_xprog {
    use std::vec;

    use super::Argument::*;
    use super::Instruction::*;
    use super::Register::*;
    use super::*;

    #[test]
    fn test_emit() {
        assert_eq!(Register::R10.emit(), "%R10");
        assert_eq!(Argument::Con(5).emit(), "$5");
        assert_eq!(Argument::Reg(Register::RAX).emit(), "%RAX");
        assert_eq!(Argument::Deref(Register::RAX, 5).emit(), "%RAX(5)");
    }

    #[test]
    fn test_xprog() {
        let test_progs: Vec<Program> =
            vec![
                vec![("_main".to_string(), vec![Movq(Con(5), Reg(RAX)), Retq])]
                    .into_iter()
                    .collect::<HashMap<_, _>>(),
            ];

        for test in test_progs {
            println!("{}", test.emit());
        }
    }
}
