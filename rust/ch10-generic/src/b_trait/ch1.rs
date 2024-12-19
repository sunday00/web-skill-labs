use std::fmt::{Debug, Display};

pub trait Summary {
    // fn summarize(&self) -> String;
    fn summarize(&self) -> String {
        String::from("(Read more...)")
    }
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

pub struct Adv {
    pub product: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{} - ({}): {}", self.headline, self.author, self.location)
    }
}

impl Summary for Tweet {
    fn summarize(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }
}

impl Summary for Adv {}

pub fn exec() {
    let tweet = Tweet {
        username: String::from("oooo"),
        content: String::from("how she there?"),
        reply: false,
        retweet: false,
    };

    println!("1 new tweet: {}", tweet.summarize());

    let adv = Adv { product: String::from("Calculator") };
    println!("2 new adv: {}", adv.summarize());

    notify(&tweet);
}

// pub fn notify(item: &impl Summary) {
//     println!("notify! {}", item.summarize());
// }

pub fn notify<T: Summary>(item: &T) {
    println!("notify! {}", item.summarize());
}

pub fn notify_double(item1: &impl Summary, item2: &impl Summary) {
    println!("notify! {} : {}", item1.summarize(), item2.summarize());
}

pub fn notify_double_same<T: Summary>(item1: &T, item2: &T) {
    println!("notify! {} : {}", item1.summarize(), item2.summarize());
}

pub fn mul_imple(item: &(impl Summary + Display)) {
    println!("notify! {}", item.summarize());
}

pub fn mul_imple2<T: Summary + Display>(item: &T) {
    println!("notify! {}", item.summarize());
}

pub fn complicated<T: Summary + Display, U: Clone + Debug>(t: &T, u: &U) {}

pub fn complicated_same<T, U>(t: &T, u: &U)
where
    T: Display + Clone,
    U: Clone + Debug,
{}

// fn returns_summarizable(switch: bool) -> impl Summary {
//     if switch {
//         // NewsArticle { // ☝️
//         //     headline: String::from(
//         //         "Penguins win the Stanley Cup Championship!",
//         //     ),
//         //     location: String::from("Pittsburgh, PA, USA"),
//         //     author: String::from("Iceburgh"),
//         //     content: String::from(
//         //         "The Pittsburgh Penguins once again are the best \
//         //          hockey team in the NHL.",
//         //     ),
//         // }
//     } else {
//         // Tweet { // ✌️
//         //     username: String::from("horse_ebooks"),
//         //     content: String::from(
//         //         "of course, as you probably already know, people",
//         //     ),
//         //     reply: false,
//         //     retweet: false,
//         // }
//     } // This is not word. if/else should same return type.
// }