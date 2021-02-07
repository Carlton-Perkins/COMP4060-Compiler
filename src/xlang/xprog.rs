use std::collections::HashMap;

type Var = usize;

type OType = i64;

type Label = String;

enum Register {
    Rsp,
    Rbp,
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

enum Argument {
    Con(OType),
    Reg(Register),
    Deref(Register, OType),
    Ref(Var),
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

type Block = Vec<Instruction>;

type Program = HashMap<Label, Block>;
