import gleam/int
import gleam/list

fn each(n: Int, target: Int) {
  // case target % n == 0 {
  //   True -> {
  //     case n {
  //       n if n == 3 -> "Pling"
  //       n if n == 5 -> "Plang"
  //       n if n == 7 -> "Plong"
  //       _ -> ""
  //     }
  //   }
  //   False -> ""
  // }

  case target % n == 0, n {
    True, n if n == 3 -> "Pling"
    True, n if n == 5 -> "Plang"
    True, n if n == 7 -> "Plong"
    _, _ -> ""
  }
}

fn reducer(acc: String, remains: List(Int), target: Int) {
  case remains {
    [] -> acc
    _ -> {
      let assert Ok(f) = remains |> list.first
      let r = remains |> list.drop(1)

      reducer(acc <> each(f, target), r, target)
    }
  }
}

pub fn convert(number: Int) -> String {
  let div = [3, 5, 7]

  let r = reducer("", div, number)

  case r {
    "" -> number |> int.to_string
    _ -> r
  }
}
// pub fn main() {
//   echo convert(30)
// }
