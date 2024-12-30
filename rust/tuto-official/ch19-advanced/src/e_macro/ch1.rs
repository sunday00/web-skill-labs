#[macro_export]
macro_rules! mini_vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();

            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

//https://doc.rust-lang.org/reference/macros-by-example.html

pub fn exec() {
    let v = mini_vec![1, 2, 3];
    println!("{:?}", v);
}
