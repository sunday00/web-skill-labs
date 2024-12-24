#[derive(Debug)]
struct Rectangle {
    width: u32,
    _height: u32,
}

pub fn exec() {
    let mut list = [
        Rectangle { width: 10, _height: 1 },
        Rectangle { width: 3, _height: 5 },
        Rectangle { width: 7, _height: 12 },
    ];

    // list.sort_by_key(|r| r.width);

    // let mut sort_operations = vec![];
    // let value = String::from("by key called");
    //
    // list.sort_by_key(|r| {
    //     sort_operations.push(value); // 🚫 value owner already moved to sort_operation. so value is unavailable on second time.
    //     r.width
    // });

    let mut num_sort_operation = 0;
    list.sort_by_key(|r| {
        num_sort_operation += 1;
        r.width
    });

    println!("{:#?}", list);
}