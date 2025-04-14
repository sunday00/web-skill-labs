// import gleam/io
import gleam/list
import gleam/result

// import gleam/result
import gleam/string

// pub fn distance(strand1: String, strand2: String) -> Result(Int, Nil) {
//   case string.length(strand1) == string.length(strand2) {
//     False -> Error(Nil)
//     True -> {
//       let r =
//         strand1
//         |> string.to_graphemes
//         |> list.fold(#(0, strand2), fn(acc, cur) {
//           // io.print("acc1: " <> acc.1)
//           // io.print("cur: " <> cur)
//           // io.print("\n")

//           let r = case acc.1 |> string.first {
//             Ok(c) if c == cur -> acc.0
//             _ -> acc.0 + 1
//           }

//           let s = acc.1 |> string.drop_left(1)

//           #(r, s)
//         })

//       Ok(r.0)
//     }
//   }
// }

pub fn distance(strand1: String, strand2: String) -> Result(Int, Nil) {
  list.strict_zip(string.to_graphemes(strand1), string.to_graphemes(strand2))
  |> result.map(fn(l) { l |> list.filter(fn(i) { i.0 != i.1 }) |> list.length })
}
// pub fn main() {
//   echo distance("GGACTGAAATCTG", "GGACTGAAATCTG")
// }
