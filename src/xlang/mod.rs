#[allow(dead_code)]
pub mod xprog;
pub use xprog::{
    XArgument, XBlock, XEnv, XInstruction, XInterpMut, XProgram, XRegister, ALL_REGISTERS,
    CALLEE_SAVED_REGISTERS, CALLER_SAVED_REGISTERS, TEMP_REGISTER,
};

#[allow(dead_code)]
mod assign_registers;
pub use assign_registers::AssignRegisters;

#[allow(dead_code)]
mod patch_instructions;
pub use patch_instructions::PatchInstructions;

#[allow(dead_code)]
mod sys_asm;
pub use sys_asm::{compile_and_run, CompileAndRun};

#[allow(dead_code)]
mod allocation;
pub use allocation::{Allocation, Allocator, GraphAllocator, StupidStackAllocator};

#[allow(dead_code)]
mod uncover_live;
