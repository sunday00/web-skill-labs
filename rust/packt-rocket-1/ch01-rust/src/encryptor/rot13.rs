// use std::error::Error;

pub struct Rot13(pub String);

impl super::Encryptable for Rot13 {
    fn encrypt(&self) -> String {
    // fn encrypt(&self) -> Result<String, Box<dyn Error>> {
        // let mut new_string = String::new();
        // let len = self.0.len();
        //
        // for i in 0..len {
        //     if (self.0[i] >= 'a' && self.0[i] < 'n') || (self.0[i] >= 'A' && self.0[i] < 'N') {
        //         new_string.push((self.0[i] as u8 + 13) as char) ;
        //     } else if (self.0[i] >= 'n' && self.0[i] < 'z') || (self.0[i] >= 'N' && self.0[i] < 'Z') {
        //         new_string.push((self.0[i] as u8 - 13) as char) ;
        //     } else {
        //         new_string.push(self.0[i]) ;
        //     }
        // }

        // for ch in self.0.chars() {
        //     if (ch >= 'a' && ch < 'n') || (ch >= 'A' && ch < 'N') {
        //         new_string.push((ch as u8 + 13) as char) ;
        //     } else if (ch >= 'n' && ch < 'z') || (ch >= 'N' && ch < 'Z') {
        //         new_string.push((ch as u8 - 13) as char) ;
        //     } else {
        //         new_string.push(ch) ;
        //     }
        // }

        // self.0.chars().map(|ch| {
        //     if (ch >= 'a' && ch < 'n') || (ch >= 'A' && ch < 'N') {
        //         (ch as u8 + 13) as char
        //     } else if (ch >= 'n' && ch < 'z') || (ch >= 'N' && ch < 'Z') {
        //         (ch as u8 - 13) as char
        //     } else {
        //         ch
        //     }
        // }).collect()

        // new_string

        self.0.chars().map(|ch| match ch {
            'a'..='m' | 'A'..='M' => (ch as u8 + 13) as char,
            'n'..='z' | 'N'..='Z' => (ch as u8 - 13) as char,
            _ => ch
        }).collect()
    }
}