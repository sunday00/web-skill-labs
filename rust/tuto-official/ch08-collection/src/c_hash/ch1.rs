use std::collections::HashMap;

pub fn exec() {
    let mut scores = HashMap::new();
    let blue = String::from("blue");
    let yellow = String::from("yellow");

    scores.insert(&blue, 10);
    scores.insert(&yellow, 50);

    let score = scores.get(&blue);
    println!("{}", score.copied().unwrap_or(0));

    for (k, v) in &scores {
        println!("{}: {}", k, v);
    }

    scores.insert(&blue, 30);
    println!("{:?}", scores);

    let green = String::from("green");
    scores.entry(&green).or_insert(10);
    scores.entry(&green).or_insert(20);
    scores.entry(&green).or_insert(30);

    println!("{:?}", scores);
}