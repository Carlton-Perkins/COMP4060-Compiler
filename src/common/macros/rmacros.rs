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

#[macro_export]
macro_rules! REQ {
    ($lh:expr, $rh:expr) => {
        crate::rlang::RExpr::RCmp(crate::rlang::RCMP::EQ, Box::new($lh), Box::new($rh))
    };
}

#[macro_export]
macro_rules! RLT {
    ($lh:expr, $rh:expr) => {
        crate::rlang::RExpr::RCmp(crate::rlang::RCMP::LT, Box::new($lh), Box::new($rh))
    };
}

#[macro_export]
macro_rules! RLEQ {
    ($lh:expr, $rh:expr) => {
        crate::rlang::RExpr::RCmp(crate::rlang::RCMP::LEQ, Box::new($lh), Box::new($rh))
    };
}

#[macro_export]
macro_rules! RGEQ {
    ($lh:expr, $rh:expr) => {
        crate::rlang::RExpr::RCmp(crate::rlang::RCMP::GEQ, Box::new($lh), Box::new($rh))
    };
}

#[macro_export]
macro_rules! RGT {
    ($lh:expr, $rh:expr) => {
        crate::rlang::RExpr::RCmp(crate::rlang::RCMP::GT, Box::new($lh), Box::new($rh))
    };
}
#[macro_export]
macro_rules! RAnd {
    ($lh:expr, $rh:expr) => {
        crate::rlang::RExpr::RCmp(crate::rlang::RCMP::AND, Box::new($lh), Box::new($rh))
    };
}
#[macro_export]
macro_rules! ROr {
    ($lh:expr, $rh:expr) => {
        crate::rlang::RExpr::RCmp(crate::rlang::RCMP::OR, Box::new($lh), Box::new($rh))
    };
}

#[macro_export]
macro_rules! RIf {
    ($c:expr, $te:expr, $fe:expr) => {
        crate::rlang::RExpr::RIf(Box::new($c), Box::new($te), Box::new($fe))
    };
}

#[macro_export]
macro_rules! RTrue {
    () => {
        crate::rlang::RExpr::RBool(true)
    };
}

#[macro_export]
macro_rules! RFalse {
    () => {
        crate::rlang::RExpr::RBool(false)
    };
}

macro_rules! RNot {
    ($e:expr) => {
        crate::rlang::RExpr::RNot(Box::new($e))
    };
}

#[cfg(test)]
mod test_rmacros {
    use crate::rlang::{RExpr::*, RCMP::*};
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
            (
                REQ!(RNum(4), RNum(4)),
                RCmp(EQ, Box::new(RNum(4)), Box::new(RNum(4))),
            ),
            (
                RLT!(RNum(3), RNum(4)),
                RCmp(LT, Box::new(RNum(3)), Box::new(RNum(4))),
            ),
            (
                RLEQ!(RNum(3), RNum(4)),
                RCmp(LEQ, Box::new(RNum(3)), Box::new(RNum(4))),
            ),
            (
                RGEQ!(RNum(3), RNum(4)),
                RCmp(GEQ, Box::new(RNum(3)), Box::new(RNum(4))),
            ),
            (
                RGT!(RNum(3), RNum(4)),
                RCmp(GT, Box::new(RNum(3)), Box::new(RNum(4))),
            ),
            (
                RIf!(RTrue!(), RTrue!(), RFalse!()),
                RIf(
                    Box::new(RBool(true)),
                    Box::new(RBool(true)),
                    Box::new(RBool(false)),
                ),
            ),
            (RNot!(RTrue!()), RNot(Box::new(RBool(true)))),
        ];

        for (e, me) in tests {
            assert_eq!(e, me, "Macro generated incorrect pattern");
        }
    }
}
