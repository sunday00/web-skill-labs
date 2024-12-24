mod common;

use ch11_testing::b_control::ch1::*;

#[test]
fn it_adds_two() {
    assert_eq!(add_two(2), 4);
}

#[test]
fn it_common() {
    common::setup()
}