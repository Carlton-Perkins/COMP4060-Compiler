use std::collections::HashMap;

use crate::clang::LocalsInfo;
use crate::common::types::{Label, Number};
use crate::xlang::{XArgument::*, XInstruction::*, XProgram, XRegister::*};

use super::{XArgument, XBlock};

pub trait AssignHomes {
    fn asn_homes(&self, linfo: &LocalsInfo) -> XProgram;
}

trait Asn {
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self;
}

impl AssignHomes for XProgram {
    fn asn_homes(&self, linfo: &LocalsInfo) -> XProgram {
        let var_count = linfo.len();
        let stack_space = 8
            * (if is_even(var_count) {
                var_count
            } else {
                var_count + 1
            });
        let renames = linfo
            .into_iter()
            .enumerate()
            .map(|(idx, label)| (label.clone(), Deref(RBP, (8 * idx) as i64)))
            .collect();
        let new_main = XBlock!(
            "main",
            Pushq(Reg(RBP)),
            Movq(Reg(RSP), Reg(RBP)),
            Subq(Con(stack_space as Number), Reg(RSP)),
            Jmp(Label!("body"))
        );
        let new_end = XBlock!(
            "end",
            Addq(Con(stack_space as Number), Reg(RSP)),
            Popq(Reg(RBP)),
            Retq
        );
        let body = self.get(&Label!("main")).unwrap();
        let new_body = (Label!("body"), body.asn(&renames));

        XProgram!(new_main, new_body, new_end)
    }
}

impl Asn for XBlock {
    fn asn(&self, renames: &HashMap<Label, XArgument>) -> Self {
        todo!()
    }
}

fn is_even(v: usize) -> bool {
    v % 2 == 0
}

#[cfg(test)]
mod test_assign_homes {
    use super::*;
    use crate::xlang::{XArgument::*, XEnv, XInstruction::*, XInterpMut, XRegister::*};

    #[test]
    fn test_assign_homes() {
        let tests: Vec<((XProgram, LocalsInfo), XProgram)> = vec![(
            (
                XProgram!(XBlock!(
                    "main",
                    Callq(Label!("_read_int")),
                    Callq(Label!("_read_int")),
                    Movq(Reg(RAX), XVar!("0")),
                    Movq(XVar!("0"), Reg(RAX)),
                )),
                vec!["0"].into_iter().map(|x| x.into()).collect(),
            ),
            XProgram!(
                XBlock!(
                    "main",
                    Pushq(Reg(RBP)),
                    Movq(Reg(RSP), Reg(RBP)),
                    Subq(Con(16), Reg(RSP)),
                    Jmp(Label!("body"))
                ),
                XBlock!("end", Addq(Con(16), Reg(RSP)), Popq(Reg(RBP)), Retq),
                XBlock!(
                    "body",
                    Callq(Label!("_read_int")),
                    Callq(Label!("_read_int")),
                    Movq(Reg(RAX), Deref(RBP, 0)),
                    Movq(Deref(RBP, 0), Reg(RAX)),
                )
            ),
        )];

        for ((prog, info), expect_prog) in tests {
            let prog_res = prog.interp(&mut XEnv::new(&prog));
            let expected_prog_res = expect_prog.interp(&mut XEnv::new(&expect_prog));
            assert_eq!(prog_res, expected_prog_res);

            let asn = prog.asn_homes(&info);
            let asn_res = asn.interp(&mut XEnv::new(&asn));
            assert_eq!(prog_res, asn_res);
            assert_eq!(asn, expect_prog);
        }
    }
}
