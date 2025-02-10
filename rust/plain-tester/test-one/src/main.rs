fn main() {
    test1();
    test2();
}

struct Somee {}

impl Somee {
    fn p(&self) {
        println!("hello");
    }

    fn pp(s: &str) {
        println!("{}", s);
    }

    // fn ff(l: &str) -> String {
    //     format!("{}", l)
    // }
}

trait Yamee {
    fn coo();
}

impl<T> Yamee for T {
    fn coo() {
        println!("jjj");
    }
}

fn test1() {
    let s = <String>::from("hello");
    let ss = String::from("hello");
    println!("{s} {ss}");

    let s = Somee {};
    s.p();

    <Somee>::pp("hello2");
    Somee::pp("hello3");
}

fn test2() {
    Somee::coo();
    <Somee>::coo();
}
