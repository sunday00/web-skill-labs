enum Message {
    Hello { id: i32 },
}

pub fn exec() {
    let msg = Message::Hello { id: 5 };

    match msg {
        Message::Hello { id: id_val @ 3..=7 } => {
            println!("id:{} : it's low", id_val);
        }
        Message::Hello { id: id_val @ 8..=12 } => {
            println!("id:{} : it's high", id_val);
        }
        Message::Hello { id: 13..=18 } => {
            println!("id : it's high");
        }
        Message::Hello { id } => {
            println!("id : {} : else ", id);
        }
    }
}