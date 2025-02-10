pub struct Message {
    pub uuid: String,
    pub orig_file_name: String,
    pub dest_file_name: String,
}
impl Message {
    pub fn new() -> Self {
        Message {
            uuid: String::new(),
            orig_file_name: String::new(),
            dest_file_name: String::new(),
        }
    }
}
