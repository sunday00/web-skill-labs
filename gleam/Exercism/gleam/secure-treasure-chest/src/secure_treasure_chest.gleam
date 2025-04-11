import gleam/string

// Please define the TreasureChest type
pub opaque type TreasureChest(a) {
  TreasureChest(password: String, treasure: a)
}

pub fn create(
  password: String,
  contents: treasure,
) -> Result(TreasureChest(treasure), String) {
  case string.length(password) < 8 {
    True -> Error("Password must be at least 8 characters long")
    False -> Ok(TreasureChest(password, contents))
  }
}

// fn chesh_pass(chest: TreasureChest(a)) -> String {
//   chest.password
// }

pub fn open(
  chest: TreasureChest(treasure),
  password: String,
) -> Result(treasure, String) {
  // let cp = chesh_pass(chest)

  case password {
    p if p == chest.password -> Ok(chest.treasure)
    _ -> Error("Incorrect password")
  }
}
