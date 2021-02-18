#[macro_export]
macro_rules! Negate {
    ($e:expr) => {
        Negate(Box::new($e))
    };
}

#[macro_export]
macro_rules! Add {
    ($lh:expr,$rh:expr) => {
        Add(Box::new($lh), Box::new($rh))
    };
}

#[macro_export]
macro_rules! Let {
    ($v:expr, $ve:expr, $be:expr) => {
        Let($v.into(), Box::new($ve), Box::new($be))
    };
}

#[macro_export]
macro_rules! Var {
    ($v:expr) => {
        Var($v.into())
    };
}

#[cfg(test)]
mod test_rmacros {
    use crate::rlang::Expr::*;

    #[test]
    fn test_rmacros() {
        let tests = vec![
            (Negate(Box::new(Num(5))), Negate!(Num(5))),
            (
                Add(Box::new(Num(5)), Box::new(Num(4))),
                Add!(Num(5), Num(4)),
            ),
            (
                Let("Foo".into(), Box::new(Num(3)), Box::new(Var("Foo".into()))),
                Let!("Foo", Num(3), Var("Foo".into())),
            ),
            (Var("Foo".into()), Var!("Foo")),
        ];

        for (e, me) in tests {
            assert_eq!(e, me, "Macro generated incorrect pattern");
        }
    }
}