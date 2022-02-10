use std::fs;
use crate::TopLevel;

use insta::assert_debug_snapshot;
use paste::paste;

macro_rules! parse_and_verify {
    ($name:ident, $path:literal) => {
        paste! {
            #[test]
            fn [<parse_and_verify_ $name>]() {
                let source = fs::read_to_string($path).unwrap();
                let ast: TopLevel = source.parse().unwrap();
                assert_debug_snapshot!(ast);
            }
        }
    };
}

parse_and_verify!(all, "src/tests/test.file");
