import gleam/float
import gleam/int

pub type Character {
  Character(
    charisma: Int,
    constitution: Int,
    dexterity: Int,
    hitpoints: Int,
    intelligence: Int,
    strength: Int,
    wisdom: Int,
  )
}

pub fn generate_character() -> Character {
  let charisma = ability()
  let constitution = ability()
  let dexterity = ability()
  let intelligence = ability()
  let strength = ability()
  let wisdom = ability()

  let hitpoint_modifier = modifier(constitution)
  let hitpoint = 10 + hitpoint_modifier

  Character(
    charisma,
    constitution,
    dexterity,
    hitpoint,
    intelligence,
    strength,
    wisdom,
  )
}

pub fn modifier(score: Int) -> Int {
  { { score |> int.to_float } -. 10.0 } /. 2.0 |> float.floor |> float.truncate
}

pub fn ability() -> Int {
  int.random(6) + int.random(6) + int.random(6) + 3
}
