// use ch14_cargo::a_base::ch1::kinds::PrimaryColor;
// use ch14_cargo::a_base::ch1::utils::mix;
use ch14_cargo::a_base::ch1::{mix, PrimaryColor};
use ch14_cargo::run;

fn main() {
    run();

    let red = PrimaryColor::Red;
    let yellow = PrimaryColor::Yellow;
    mix(red, yellow);
}