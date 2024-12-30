// fn generic<T>(t: T) {
//     // --생략--
// }

fn generic<T: ?Sized>(t: &T) {
    // --생략--
}

pub fn exec() {}
