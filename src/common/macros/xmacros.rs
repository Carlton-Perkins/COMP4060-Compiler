#[macro_export]
macro_rules! XProgram {
    ($($blk:expr),+ $(,)?) => {
        <[_]>::into_vec(Box::new([$($blk),+])).into_iter().collect::<XProgram>()
    };
}

#[macro_export]
macro_rules! XBlock {
    ($str:expr, $($blk:expr),+ $(,)?) => {
        // Serious black magic going on here
        // Derived from <https://doc.rust-lang.org/src/alloc/macros.rs.html#41-51>
        ($str.to_string(), <[_]>::into_vec(Box::new([$($blk),+])))
    };
}

macro_rules! XVar {
    ($str:expr) => {
        crate::xlang::XArgument::Var($str.into())
    };
}

#[cfg(test)]
mod test_xmacros {
    use crate::common::types::Label;
    use crate::xlang::{XArgument::*, XBlock, XInstruction::*, XProgram, XRegister::*};
    use std::collections::HashMap;
    #[test]
    fn test_xprograms() {
        let test_progs: Vec<(XProgram, XProgram)> = vec![(
            vec![
                ("foo".to_string(), vec![Movq(Con(33), Reg(RAX)), Retq]),
                ("main".to_string(), vec![Jmp("foo".to_string()), Retq]),
            ]
            .into_iter()
            .collect::<HashMap<_, _>>(),
            XProgram!(
                XBlock!("foo", Movq(Con(33), Reg(RAX)), Retq),
                XBlock!("main", Jmp(Label!("foo")), Retq)
            ),
        )];

        for (expected, given) in test_progs {
            assert_eq!(expected, given);
        }
    }
    #[test]
    fn test_xblocks() {
        let tests: Vec<((Label, XBlock), (Label, XBlock))> = vec![(
            (
                "main".to_string(),
                vec![
                    Movq(Con(5), Reg(RAX)),
                    Movq(Con(6), Reg(R9)),
                    Pushq(Reg(R9)),
                    Popq(Reg(RAX)),
                    Retq,
                ],
            ),
            XBlock!(
                "main",
                Movq(Con(5), Reg(RAX)),
                Movq(Con(6), Reg(R9)),
                Pushq(Reg(R9)),
                Popq(Reg(RAX)),
                Retq,
            ),
        )];

        for (expected_prog, test_prog) in tests {
            assert_eq!(expected_prog, test_prog);
        }
    }
}
