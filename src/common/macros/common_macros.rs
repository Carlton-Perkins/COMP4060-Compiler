#[macro_export]
macro_rules! Label {
    ($e:expr) => {
        crate::common::types::Label::from($e)
    };
}

#[macro_export]
macro_rules! set {
    ($($blk:expr),+ $(,)?) => {
        <[_]>::into_vec(Box::new([$($blk),+])).into_iter().collect::<std::collections::HashSet<_>>()

    };
    () => {
        std::collections::HashSet::new()
    }
}

#[cfg(test)]
mod test_cmacros {

    use crate::common::types::Label;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_cmacros() {
        assert_eq!(Label!("main"), Label::from("main"))
    }

    #[test]
    fn test_set() {
        assert_eq!(
            vec!["x"]
                .into_iter()
                .collect::<std::collections::HashSet<_>>(),
            set!["x"]
        );
        assert_eq!(
            vec![]
                .into_iter()
                .collect::<std::collections::HashSet<String>>(),
            set![]
        );
        assert_eq!(
            vec!["x", "y"]
                .into_iter()
                .collect::<std::collections::HashSet<_>>(),
            set!["x", "y"]
        );
        assert_eq!(
            vec!["x", "x"]
                .into_iter()
                .collect::<std::collections::HashSet<_>>(),
            set!["x"]
        );
    }
}
