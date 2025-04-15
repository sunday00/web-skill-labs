import gleam/int
import gleam/string

pub type Resistance {
  Resistance(unit: String, value: Int)
}

fn to_numeric(s: String) {
  case s {
    "black" -> "0"
    "brown" -> "1"
    "red" -> "2"
    "orange" -> "3"
    "yellow" -> "4"
    "green" -> "5"
    "blue" -> "6"
    "violet" -> "7"
    "grey" -> "8"
    "white" -> "9"
    _ -> "-1"
  }
}

fn to_unit(s: String) {
  let assert Ok(v) = s |> int.parse

  case v {
    v if v < 1000 -> Ok(Resistance("ohms", v))
    v if v >= 1000 && v < 1_000_000 -> Ok(Resistance("kiloohms", v / 1000))
    v if v >= 1_000_000 && v < 1_000_000_000 ->
      Ok(Resistance("megaohms", v / 1_000_000))
    _ -> Ok(Resistance("gigaohms", v / 1_000_000_000))
  }
}

pub fn label(colors: List(String)) -> Result(Resistance, Nil) {
  let assert [a, b, c, ..] = colors
  let assert Ok(repeat) = c |> to_numeric |> int.parse

  { to_numeric(a) <> to_numeric(b) <> string.repeat("0", repeat) } |> to_unit
}
// pub fn main() {
//   echo label(["red", "black", "red"])
// }
