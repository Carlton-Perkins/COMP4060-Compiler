use compiler::{
    clang::{CEnv, SelectInstruction, UncoverLocals},
    common::traits::{Emit, InterpMut},
    rlang::{randp, ECEnv, ExplicateControl, REnv, RandEnv, ResolveComplex, UEnv, Uniquify},
    xlang::{AssignHomes, CompileAndRun, PatchInstructions, XEnv, XInterpMut},
};

fn main() {
    let depth = 3;
    // RLang
    let e = randp(depth, &RandEnv::new());
    println!("RProgram: \n\n{:?}\n", e);
    let e_ret = e.interp(&mut REnv::new());

    let u = e.uniquify(&mut UEnv::new());
    let u_ret = u.interp(&mut REnv::new());
    assert_eq!(e_ret, u_ret);

    let rco = u.resolve_complex();
    let rco_ret = rco.interp(&mut REnv::new());
    assert_eq!(e_ret, rco_ret);

    let econ = rco.explicate_control(ECEnv::new());
    let econ_ret = econ.interp(&mut CEnv::new(&econ));
    assert_eq!(e_ret, econ_ret);

    // CLang
    let (ul, local_info) = econ.uncover_locals();
    let ul_ret = ul.interp(&mut CEnv::new(&ul));
    assert_eq!(e_ret, ul_ret);

    let sel_inst = ul.select_instr();
    let sel_inst_ret = sel_inst.interp(&mut XEnv::new(&sel_inst));
    assert_eq!(e_ret, sel_inst_ret);

    // XLang
    let asn = sel_inst.asn_homes(&local_info);
    let asn_ret = asn.interp(&mut XEnv::new(&asn));
    assert_eq!(e_ret, asn_ret);

    let patch = asn.patch();
    let patch_ret = patch.interp(&mut XEnv::new(&patch));
    assert_eq!(e_ret, patch_ret);

    let sys_res = patch.run();
    assert_eq!(e_ret, sys_res);

    println!("ASM Program: \n\n{}\n", patch.emit());
}
