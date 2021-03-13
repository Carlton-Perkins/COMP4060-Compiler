use compiler::{
    clang::{SelectInstruction, UncoverLocals},
    common::traits::{Emit, InterpMut},
    rlang::{randp, ExplicateControl, ResolveComplex, Uniquify},
    xlang::{AssignRegisters, CompileAndRun, PatchInstructions, StupidStackAllocator, XInterpMut},
};

fn main() {
    let depth = 3;
    // RLang
    let e = randp(depth);
    println!("RProgram: \n\n{:?}\n", e);
    let e_ret = e.interp().unwrap();

    let u = e.uniquify();
    let u_ret = u.interp().unwrap();
    assert_eq!(e_ret, u_ret);

    let rco = u.resolve_complex();
    let rco_ret = rco.interp().unwrap();
    assert_eq!(e_ret, rco_ret);

    let econ = rco.explicate_control();
    let econ_ret = econ.interp();
    assert_eq!(e_ret, econ_ret);

    // CLang
    let (ul, local_info) = econ.uncover_locals();
    let ul_ret = ul.interp();
    assert_eq!(e_ret, ul_ret);

    let sel_inst = ul.select_instr();
    let sel_inst_ret = sel_inst.interp();
    assert_eq!(e_ret, sel_inst_ret);

    // XLang
    let asn = sel_inst.asn_registers(&local_info, StupidStackAllocator {});
    let asn_ret = asn.interp();
    assert_eq!(e_ret, asn_ret);

    let patch = asn.patch();
    let patch_ret = patch.interp();
    assert_eq!(e_ret, patch_ret);

    let sys_res = patch.run();
    assert_eq!(e_ret, sys_res);

    println!("ASM Program: \n\n{}\n", patch.emit());
}
