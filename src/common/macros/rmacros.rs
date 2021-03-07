#[macro_export]
macro_rules! RNegate {
    ($e:expr) => {
        crate::rlang::RExpr::RNegate(Box::new($e))
    };
}

#[macro_export]
macro_rules! RAdd {
    ($lh:expr,$rh:expr) => {
        crate::rlang::RExpr::RAdd(Box::new($lh), Box::new($rh))
    };
}

#[macro_export]
macro_rules! RLet {
    ($v:expr, $ve:expr, $be:expr) => {
        crate::rlang::RExpr::RLet($v.into(), Box::new($ve), Box::new($be))
    };
}

#[macro_export]
macro_rules! RVar {
    ($v:expr) => {
        crate::rlang::RExpr::RVar($v.into())
    };
}

#[cfg(test)]
mod test_rmacros {
    use crate::rlang::RExpr::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_rmacros() {
        let tests = vec![
            (RNegate(Box::new(RNum(5))), RNegate!(RNum(5))),
            (
                RAdd(Box::new(RNum(5)), Box::new(RNum(4))),
                RAdd!(RNum(5), RNum(4)),
            ),
            (
                RLet(
                    "Foo".into(),
                    Box::new(RNum(3)),
                    Box::new(RVar("Foo".into())),
                ),
                RLet!("Foo", RNum(3), RVar("Foo".into())),
            ),
            (RVar("Foo".into()), RVar!("Foo")),
        ];

        for (e, me) in tests {
            assert_eq!(e, me, "Macro generated incorrect pattern");
        }
    }
}
