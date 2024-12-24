use std::fmt::Display;

struct Pair<T> {
    x: T,
    y: T,
}

impl<T> Pair<T> {
    fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

pub trait ToDoubleString {
    fn to_string(&self) -> String;
}

impl<T: Display> ToDoubleString for T { // implements apply to all USING TRAIT
    fn to_string(&self) -> String {
        format!("{}", self)
    }
}

impl<T: Display + PartialOrd> Pair<T> {
    fn cmp_display(&self) {
        if self.x >= self.y {} else {}
    }
}

pub fn exec() {}


// Trait can apply struct -> like interface
// can be argument -> like type
// can be return type -> abstract returning but implemented type should always same
// cam be applied OTHER TRAIT IMPLEMENTED -> when A trait applied, also B trait apply auto.