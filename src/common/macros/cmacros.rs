#[macro_export]
macro_rules! CProgram {
    ($($blk:expr),+ $(,)?) => {
        // Serious black magic going on here
        // Derived from <https://doc.rust-lang.org/src/alloc/macros.rs.html#41-51>
        <[_]>::into_vec(Box::new([$($blk),+])).into_iter().collect::<CProgram>()
    };
}

#[macro_export]
macro_rules! CTail {
    ($str:expr, $prog:expr) => {
        ($str.into(), $prog)
    };
}

#[macro_export]
macro_rules! CVar {
    ($v:expr) => {
        crate::clang::CArgument::Var($v.into())
    };
}

#[macro_export]
macro_rules! CSet {
    ($v:tt, $e:expr) => {
        crate::clang::CStatement::Set($v.into(), $e)
    };
}

#[macro_export]
macro_rules! CSeq {
    ($fst:expr, $sec:expr) => {
        crate::clang::CTail::Seq($fst, Box::new($sec))
    };
}

#[cfg(test)]
mod test_cmacros {
    use crate::clang::{CArgument, CExpression, CProgram, CStatement, CTail};
    use CArgument::*;
    use CExpression::*;
    use CStatement::*;
    use CTail::*;

    #[test]
    fn test_cmacros() {
        let tests = vec![(
            vec![(
                Label!("main"),
                Seq(
                    Set("0".into(), Arg(Num(5))),
                    Box::new(Return(Var("0".into()))),
                ),
            )]
            .into_iter()
            .collect::<CProgram>(),
            CProgram!(CTail!(
                "main",
                CSeq!(CSet!("0", Arg(Num(5))), Return(CVar!("0")))
            )),
        )];

        for (start, end) in tests {
            assert_eq!(start, end);
        }
    }
}
