import gleam/int
import gleam/list
import gleam/option
import gleam/regexp
import gleam/result
import gleam/string

pub type Error {
  SyntaxError
  UnknownOperation
  ImpossibleOperation
}

pub fn answer(question: String) -> Result(Int, Error) {
  let assert Ok(h_patt) = regexp.from_string("What is (-{0,1}\\d+)")
  let assert Ok(t_patt) =
    regexp.from_string("((plus|minus|multiplied by|divided by) (-{0,1}\\d+))")

  case get_igniting(question) {
    option.Some(n) -> {
      let head = int.parse(n)

      case head {
        Ok(h) -> {
          let tails = regexp.scan(t_patt, question)

          case tails |> list.length > 0 {
            True -> {
              let res = reducer(h, tails)

              case is_remain_after_dn(question, h_patt, t_patt) {
                True -> Error(SyntaxError)
                False -> res
              }
            }
            False -> {
              case
                is_tail_start_with_operator(h_patt, question),
                is_just_number(question, h_patt)
              {
                True, _ -> Error(SyntaxError)
                False, True -> Ok(h)
                False, False -> Error(UnknownOperation)
              }
            }
          }
        }
        _ -> Error(UnknownOperation)
      }
    }
    option.None -> {
      case question |> string.contains("What is") {
        True -> Error(SyntaxError)
        False -> Error(UnknownOperation)
      }
    }
  }
}

fn get_igniting(str: String) {
  let assert Ok(patt) = regexp.from_string("What is (-{0,1}\\d+)")
  case regexp.scan(patt, str) {
    [regexp.Match(_, n), ..] -> {
      let assert [m] = n
      m
    }
    [] -> option.None
  }
}

fn reducer(acc: Int, remains: List(regexp.Match)) {
  case remains {
    [f, ..r] -> {
      let regexp.Match(_, l) = f
      let assert [_, option.Some(op), option.Some(i)] = l

      let i = i |> int.parse |> result.unwrap(0)

      case op {
        "plus" -> reducer(acc + i, r)
        "minus" -> reducer(acc - i, r)
        "multiplied by" -> reducer(acc * i, r)
        "divided by" -> {
          case i {
            0 -> Error(ImpossibleOperation)
            _ -> reducer(acc / i, r)
          }
        }
        _ -> Error(UnknownOperation)
      }
    }
    [] -> Ok(acc)
  }
}

fn is_tail_start_with_operator(h_patt: regexp.Regexp, question: String) {
  let remain_tail = regexp.replace(h_patt, question, "")

  remain_tail |> string.contains("plus")
  || remain_tail |> string.contains("minus")
  || remain_tail |> string.contains("multiplied")
  || remain_tail |> string.contains("divided")
}

fn is_remain_after_dn(
  question: String,
  h_patt: regexp.Regexp,
  t_patt: regexp.Regexp,
) {
  let remains =
    regexp.replace(h_patt, question, "")
    |> regexp.replace(t_patt, _, "")
    |> string.replace("?", "")
    |> string.trim

  remains |> string.length > 0
}

fn is_just_number(question: String, h_patt: regexp.Regexp) {
  let remains =
    regexp.replace(h_patt, question, "")
    |> string.replace("?", "")
    |> string.trim

  remains |> string.length == 0
}
// pub fn main() {
//   echo answer("What is 5?")
// }
