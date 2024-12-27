pub fn exec() {
    let mut setting_value = Some(5);
    let new_setting_value = Some(10);

    match (setting_value, new_setting_value) {
        (Some(_), Some(_)) => {
            println!("Can't overwrite an existing customized value");
        }
        _ => {
            setting_value = new_setting_value;
        }
    }

    println!("setting is {:?}", setting_value);
}

pub fn exec2() {
    let s = Some(String::from("Hello!"));

    if let Some(_s) = s { // ⚠️ _s is bindable. so, s moved.
        println!("found a string");
    }

    // println!("{:?}", s); // 🚫 s moved  already.

    let s = Some(String::from("Hello!"));

    if let Some(_) = s { // ⚠️ _ ignores value. so, s NOT moved.
        println!("found a string");
    }

    println!("{:?}", s); // ✅
}