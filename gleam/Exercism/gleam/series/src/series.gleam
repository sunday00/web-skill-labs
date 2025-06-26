import gleam/list
import gleam/string

pub fn slices(input: String, size: Int) -> Result(List(String), Error) {
  let n = string.length(input)

  case n {
    n if n == 0 -> Error(EmptySeries)
    _ -> {
      case size {
        m if m == 0 -> Error(SliceLengthZero)
        m if m < 0 -> Error(SliceLengthNegative)
        m if m > n -> Error(SliceLengthTooLarge)
        // _ -> Ok(reducer(input, [], size, 0))
        _ -> Ok(reducer(input, size))
      }
    }
  }
}

// pub fn reducer(inp: String, res: List(String), n: Int, ind: Int) {
//   let len = string.length(inp)
//   let right_drop = len - { ind + n }
//   let s = inp |> string.drop_start(ind) |> string.drop_end(right_drop)

//   case string.length(s) {
//     r if r == n -> {
//       reducer(inp, list.append(res, [s]), n, ind + 1)
//     }
//     _ -> res
//   }
// }

pub fn reducer(inp: String, n: Int) {
  inp |> string.to_graphemes |> list.window(n) |> list.map(string.concat)
}

pub type Error {
  SliceLengthTooLarge
  SliceLengthZero
  SliceLengthNegative
  EmptySeries
}
// pub fn main() {
//   slices("49142", 3)
// }
