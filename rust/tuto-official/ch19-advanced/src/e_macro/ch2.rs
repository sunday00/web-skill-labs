// #[some_attribute]
// pub fn some_name(input: TokenStream) -> TokenStream {}

use hello_macro::HelloMacro;

struct Pancakes;

// impl HelloMacro for Pancakes {
//     fn hello_macro() {
//         println!("Hello, Macro! My name is Pancakes!");
//     }
// }

pub fn exec() {
    HelloMacro::hello_macro();
}
