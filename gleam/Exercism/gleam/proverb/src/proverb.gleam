import gleam/list
import gleam/string

pub fn recite(inputs: List(String)) -> String {
  case inputs {
    [] -> ""
    [f, ..r] -> {
      case r {
        [] -> ["And all for the want of a ", f, "."] |> string.concat
        [s] -> {
          [
            generator(f, s),
            ["And all for the want of a ", f, "."] |> string.concat,
          ]
          |> string.join("\n")
        }
        [_s, ..] -> {
          let map = inputs |> list.index_map(fn(n, i) { #(i, n) })

          map
          |> list.map(fn(m) {
            let #(k, v) = m
            case map |> list.key_find(k + 1) {
              Ok(s) -> {
                generator(v, s)
              }
              Error(_) -> {
                ["And all for the want of a ", f, "."] |> string.concat
              }
            }
          })
          |> string.join("\n")
        }
      }
    }
  }
}

fn generator(f: String, s: String) {
  ["For want of a ", f, " the ", s, " was lost."] |> string.concat
}
// pub fn main() {
//   echo recite(["nail", "shoe", "horse", "rider", "message", "battle", "kingdom"])
// }
