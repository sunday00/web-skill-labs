import gleam/int

pub type Color {
  Black
  Brown
  Red
  Orange
  Yellow
  Green
  Blue
  Violet
  Grey
  White
}

fn to_i(c: Color) -> String {
  case c {
    Black -> "0"
    Brown -> "1"
    Red -> "2"
    Orange -> "3"
    Yellow -> "4"
    Green -> "5"
    Blue -> "6"
    Violet -> "7"
    Grey -> "8"
    White -> "9"
  }
}

pub fn value(colors: List(Color)) -> Result(Int, Nil) {
  case colors {
    [c1, c2, ..] -> {
      { c1 |> to_i <> c2 |> to_i } |> int.parse
    }
    _ -> Error(Nil)
  }
}
