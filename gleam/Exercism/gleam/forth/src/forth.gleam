import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Forth {
  Forth(commands: dict.Dict(String, List(String)), stack: List(String))
}

pub type ForthError {
  DivisionByZero
  StackUnderflow
  InvalidWord
  UnknownWord
}

pub fn new() -> Forth {
  Forth(dict.new(), [])
}

pub fn format_stack(f: Forth) -> String {
  f.stack |> string.join(" ")
}

pub fn eval(f: Forth, prog: String) -> Result(Forth, ForthError) {
  let inputs = prog |> string.lowercase |> string.split(" ")

  case inputs {
    [":", w, ..r] -> {
      define_word(w, f, r |> list.take(list.length(r) - 1))
    }
    _ -> {
      reducer(f, inputs)
    }
  }
}

fn define_word(w: String, forth: Forth, inputs: List(String)) {
  use <- bool.guard(
    when: w |> int.parse |> result.is_ok,
    return: Error(InvalidWord),
  )

  inputs
  |> list.flat_map(fn(inp) {
    dict.get(forth.commands, inp) |> result.unwrap([inp])
  })
  |> dict.insert(forth.commands, w, _)
  |> fn(commands) { Ok(Forth(..forth, commands: commands)) }
}

fn reducer(acc: Forth, remains: List(String)) {
  case remains |> list.length > 0 {
    True -> {
      let assert [f, ..r] = remains

      let cmd = dict.get(acc.commands, f)
      case cmd {
        Ok(c) -> {
          reducer(acc, c |> list.append(r))
        }
        _ -> {
          case f {
            "+" -> {
              case acc.stack |> list.reverse {
                [el2, el1, ..rest] -> {
                  let v =
                    { el1 |> int.parse |> result.unwrap(0) }
                    + { el2 |> int.parse |> result.unwrap(0) }
                    |> int.to_string()
                  reducer(
                    Forth(
                      acc.commands,
                      rest |> list.reverse |> list.append([v]),
                    ),
                    r,
                  )
                }
                _ -> Error(StackUnderflow)
              }
            }
            "*" -> {
              case acc.stack |> list.reverse {
                [el2, el1, ..rest] -> {
                  let v =
                    { el1 |> int.parse |> result.unwrap(0) }
                    * { el2 |> int.parse |> result.unwrap(0) }
                    |> int.to_string()
                  reducer(
                    Forth(
                      acc.commands,
                      rest |> list.reverse |> list.append([v]),
                    ),
                    r,
                  )
                }
                _ -> Error(StackUnderflow)
              }
            }
            "-" -> {
              case acc.stack |> list.reverse {
                [el2, el1, ..rest] -> {
                  let v =
                    { el1 |> int.parse |> result.unwrap(0) }
                    - { el2 |> int.parse |> result.unwrap(0) }
                    |> int.to_string()
                  reducer(
                    Forth(
                      acc.commands,
                      rest |> list.reverse |> list.append([v]),
                    ),
                    r,
                  )
                }
                _ -> Error(StackUnderflow)
              }
            }
            "/" -> {
              case acc.stack |> list.reverse {
                [el2, el1, ..rest] -> {
                  case el2 {
                    n if n == "0" -> Error(DivisionByZero)
                    _ -> {
                      let v =
                        { el1 |> int.parse |> result.unwrap(0) }
                        / { el2 |> int.parse |> result.unwrap(0) }
                        |> int.to_string()
                      reducer(
                        Forth(
                          acc.commands,
                          rest |> list.reverse |> list.append([v]),
                        ),
                        r,
                      )
                    }
                  }
                }
                _ -> Error(StackUnderflow)
              }
            }
            "dup" -> {
              case acc.stack |> list.last {
                Ok(target) -> {
                  reducer(
                    Forth(acc.commands, acc.stack |> list.append([target])),
                    r,
                  )
                }
                _ -> Error(StackUnderflow)
              }
            }
            "drop" -> {
              case acc.stack |> list.reverse {
                [_] -> reducer(Forth(acc.commands, []), r)
                [_, ..rest] ->
                  reducer(Forth(acc.commands, rest |> list.reverse), r)
                [] -> Error(StackUnderflow)
              }
            }
            "swap" -> {
              case acc.stack |> list.reverse {
                [el2, el1, ..rest] -> {
                  reducer(
                    Forth(
                      acc.commands,
                      rest |> list.reverse |> list.append([el2, el1]),
                    ),
                    r,
                  )
                }
                _ -> Error(StackUnderflow)
              }
            }
            "over" -> {
              case acc.stack |> list.reverse |> list.drop(1) |> list.first {
                Ok(target) -> {
                  reducer(
                    Forth(acc.commands, acc.stack |> list.append([target])),
                    r,
                  )
                }
                _ -> Error(StackUnderflow)
              }
            }
            _ -> {
              case int.parse(f) {
                Ok(_) ->
                  reducer(Forth(acc.commands, acc.stack |> list.append([f])), r)
                _ -> Error(UnknownWord)
              }
            }
          }
        }
      }
    }
    False -> Ok(acc)
  }
}
