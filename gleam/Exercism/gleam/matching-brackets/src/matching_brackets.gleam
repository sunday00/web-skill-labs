import gleam/list
import gleam/regexp
import gleam/string

pub fn is_paired(value: String) -> Bool {
  let assert Ok(patt) = regexp.from_string("\\[|\\]|\\{|\\}|\\(|\\)")

  let summaries =
    regexp.scan(patt, value) |> list.map(fn(l) { l.content }) |> string.join("")

  let braces = summaries |> string.to_graphemes()

  reducer([], braces)
}

fn reducer(should_be: List(String), entire: List(String)) {
  case entire |> list.first() {
    Ok(cur) -> {
      let new_entire = entire |> list.drop(1)

      case cur {
        ch if ch == "(" -> reducer(should_be |> list.append([")"]), new_entire)
        ch if ch == "{" -> reducer(should_be |> list.append(["}"]), new_entire)
        ch if ch == "[" -> reducer(should_be |> list.append(["]"]), new_entire)
        ch if ch == ")" || ch == "}" || ch == "]" -> {
          let pop = should_be |> list.last()
          case pop {
            Error(_) -> False
            Ok(l) -> {
              case l {
                l if l == ch -> {
                  reducer(
                    should_be
                      |> string.join("")
                      |> string.drop_end(1)
                      |> string.to_graphemes(),
                    new_entire,
                  )
                }
                l if l != ch -> False
                _ -> False
              }
            }
          }
        }
        _ -> {
          False
        }
      }
    }
    Error(_) -> {
      case should_be |> list.length {
        0 -> True
        _ -> False
      }
    }
  }
}
// pub fn main() {
//   echo is_paired(
//     "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)",
//   )
// }
