import gleam/list
import gleam/result
import gleam/string

pub type Student {
  Alice
  Bob
  Charlie
  David
  Eve
  Fred
  Ginny
  Harriet
  Ileana
  Joseph
  Kincaid
  Larry
}

pub fn to_student() -> List(Student) {
  [
    Alice,
    Bob,
    Charlie,
    David,
    Eve,
    Fred,
    Ginny,
    Harriet,
    Ileana,
    Joseph,
    Kincaid,
    Larry,
  ]
}

pub type Plant {
  Radishes
  Clover
  Violets
  Grass
}

pub fn to_plant() -> List(Plant) {
  [Radishes, Clover, Violets, Grass]
}

pub fn plants(diagram: String, student: Student) -> List(Plant) {
  let students = to_student() |> list.index_map(fn(x, i) { #(x, i) })
  let s_idx = students |> list.key_find(student) |> result.unwrap(0)

  let #(front_plants, behind_plants) = case diagram |> string.split("\n") {
    [front_plants, behind_plants] -> #(front_plants, behind_plants)
    _ -> {
      #("", "")
    }
  }

  let s_plants =
    []
    |> list.append([
      front_plants
        |> string.to_graphemes()
        |> list.index_map(fn(x, i) { #(i, x) })
        |> list.key_find(s_idx * 2)
        |> result.unwrap(""),
      front_plants
        |> string.to_graphemes()
        |> list.index_map(fn(x, i) { #(i, x) })
        |> list.key_find(s_idx * 2 + 1)
        |> result.unwrap(""),
      behind_plants
        |> string.to_graphemes()
        |> list.index_map(fn(x, i) { #(i, x) })
        |> list.key_find(s_idx * 2)
        |> result.unwrap(""),
      behind_plants
        |> string.to_graphemes()
        |> list.index_map(fn(x, i) { #(i, x) })
        |> list.key_find(s_idx * 2 + 1)
        |> result.unwrap(""),
    ])

  s_plants
  |> list.map(fn(l) {
    case l {
      n if n == "R" -> Radishes
      n if n == "C" -> Clover
      n if n == "V" -> Violets
      _ -> Grass
    }
  })
}
// pub fn main() {
//   plants("VVCCGG\nVVCCGG", Bob)
// }
