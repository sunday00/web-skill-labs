use std::error::Error;
use rand::rngs::OsRng;
use rsa::{Pkcs1v15Encrypt, RsaPrivateKey};

const KEY_SIZE: usize = 2048;

pub struct Rsa {
    data: String,
    private_key: RsaPrivateKey,
}

impl Rsa {
    pub fn new(input: String) -> Result<Self, Box<dyn Error>> {
        let mut rng = OsRng;
        let private_key = RsaPrivateKey::new(&mut rng, KEY_SIZE)?;
        let public_key = private_key.to_public_key();
        let input_bytes = input.as_bytes();

        let encrypted_data = public_key.encrypt(&mut rng, Pkcs1v15Encrypt, input_bytes)?;
        let encoded_data = base64::encode(encrypted_data);

        Ok(Self { data: encoded_data, private_key })
    }
}

impl super::Cipher for Rsa {
    fn original_string(&self) -> Result<String, Box<dyn Error>> {
        let decoded_data = base64::decode(&self.data).unwrap();
        let decrypted_data = self.private_key.decrypt(Pkcs1v15Encrypt, &decoded_data).unwrap();

        Ok(String::from_utf8(decrypted_data).unwrap())
    }

    fn encrypted_string(&self) -> Result<String, Box<dyn Error>> {
        Ok(String::from(&self.data))
    }
}