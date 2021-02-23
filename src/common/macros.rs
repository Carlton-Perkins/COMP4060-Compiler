#[macro_export]
macro_rules! Label {
    ($e:expr) => {
        crate::common::types::Label::from($e)
    };
}

#[cfg(test)]
mod test_cmacros {
    use crate::common::types::Label;

    #[test]
    fn test_cmacros() {
        assert_eq!(Label!("main"), Label::from("main"))
    }
}
