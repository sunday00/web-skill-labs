import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/set.{type Set}
import gleam/string

type Puzzle {
  Puzzle(
    puzzle: String,
    left: List(String),
    right: String,
    all_i: Set(Int),
    all_words: List(String),
    all_chars: Set(String),
    no_zeros: List(String),
    max_word_len: Int,
    max_left_len: Int,
  )
}

pub fn solve(puzzle: String) -> Result(Dict(String, Int), Nil) {
  let assert [left, right] = puzzle |> string.split(" == ")
  let all_i = list.range(0, 9) |> set.from_list
  let all_words = left |> string.split(" + ") |> list.prepend(right)
  let all_chars =
    all_words
    |> list.fold([], fn(acc, cur) {
      acc |> list.append(cur |> string.to_graphemes)
    })
    |> set.from_list

  let no_zeros =
    all_words |> list.map(fn(el) { el |> string.first |> result.unwrap("") })

  let puzzle =
    Puzzle(
      puzzle,
      left |> string.split(" + "),
      right,
      all_i,
      all_words,
      all_chars,
      no_zeros,
      max_word_len: all_words
        |> list.fold(0, fn(acc, cur) { acc |> int.max(cur |> string.length) }),
      max_left_len: left
        |> string.split(" + ")
        |> list.fold(0, fn(acc, el) { int.max(acc, el |> string.length) }),
    )

  resolve(puzzle)
}

fn resolve(puzzle: Puzzle) {
  case processing(puzzle, dict.new(), option.None, [], 1) {
    Ok(p) -> {
      Ok(p |> dict.map_values(fn(_, v) { v.0 }))
    }
    _ -> Error(Nil)
  }
}

fn processing(
  puzzle: Puzzle,
  fixed_dict: Dict(String, #(Int, List(Int))),
  snap_shot: option.Option(Dict(String, #(Int, List(Int)))),
  snap_shots: List(#(Dict(String, #(Int, List(Int))), Int)),
  col: Int,
) {
  case puzzle.max_word_len < col {
    True -> {
      Ok(fixed_dict)
    }
    False -> {
      let parts = extract_tail(puzzle, col)

      let indexd_map = map_idx_unq(parts, fixed_dict)

      let state = case snap_shot {
        option.Some(s) -> {
          get_next_state(
            puzzle,
            parts,
            indexd_map,
            s |> dict.drop(fixed_dict |> dict.keys),
            fixed_dict,
            col,
            { { s |> dict.drop(fixed_dict |> dict.keys) } |> dict.size } - 1,
          )
        }
        option.None -> {
          case indexd_map.0 |> list.length == 0 {
            True -> {
              let paragraph = map_i_to_parts(parts, fixed_dict)

              case is_correct_parts(paragraph, col) {
                True -> Ok(fixed_dict)
                False -> Error(Nil)
              }
            }
            False -> {
              get_next_state(
                puzzle,
                parts,
                indexd_map,
                dict.new(),
                fixed_dict,
                col,
                0,
              )
            }
          }
        }
      }

      case state {
        Ok(s) -> {
          processing(
            puzzle,
            s,
            option.None,
            snap_shots |> list.prepend(#(s, col)),
            col + 1,
          )
        }
        Error(_) -> {
          case snap_shots {
            [ls] -> {
              let #(fixed_dict, col) = ls

              processing(puzzle, dict.new(), option.Some(fixed_dict), [], col)
            }
            [ls, ls2, ..r] -> {
              let #(fixed_dict, col) = ls
              let #(fixed_dict2, _col2) = ls2

              processing(
                puzzle,
                fixed_dict2,
                option.Some(fixed_dict),
                [ls2, ..r],
                col,
              )
            }

            _ -> {
              Error(Nil)
            }
          }
        }
      }
    }
  }
}

fn get_next_state(
  puzzle: Puzzle,
  parts: #(List(String), String),
  indexd_map: #(List(#(Int, String)), List(#(String, Int))),
  state: Dict(String, #(Int, List(Int))),
  fixed_dict: Dict(String, #(Int, List(Int))),
  col: Int,
  id: Int,
) {
  let #(i_ch, _) = indexd_map
  let state = allocate_next_num(id, state, fixed_dict, indexd_map, puzzle)

  case state {
    Ok(s) -> {
      let ms = dict.merge(s, fixed_dict)

      let paragraph = map_i_to_parts(parts, ms)

      case is_correct_parts(paragraph, col) {
        True -> {
          Ok(ms)
        }
        False -> {
          get_next_state(
            puzzle,
            parts,
            indexd_map,
            s,
            fixed_dict,
            col,
            list.length(i_ch) - 1,
          )
        }
      }
    }
    Error(_) -> {
      Error(Nil)
    }
  }
}

fn extract_tail(puzzle: Puzzle, col: Int) {
  let left_tails =
    puzzle.left
    |> list.map(fn(el) {
      let col = case col > string.length(el) {
        True -> string.length(el)
        False -> col
      }
      el |> string.slice(col * -1, col)
    })

  #(left_tails, puzzle.right |> string.slice(col * -1, col))
}

fn map_idx_unq(
  parts: #(List(String), String),
  fixed_dict: Dict(String, #(Int, List(Int))),
) {
  let #(ll, r) = parts
  let unique_all_chars =
    ll
    |> list.map(fn(el) { el |> string.to_graphemes })
    |> list.flatten
    |> list.append(r |> string.to_graphemes)
    |> list.unique

  let unique_all_chars =
    unique_all_chars
    |> set.from_list
    |> set.difference(
      fixed_dict
      |> dict.keys
      |> set.from_list,
    )
    |> set.to_list
    |> list.sort(string.compare)

  #(
    unique_all_chars |> list.index_map(fn(ch, i) { #(i, ch) }),
    unique_all_chars |> list.index_map(fn(ch, i) { #(ch, i) }),
  )
}

fn get_avail_remains_ints(
  puzzle: Puzzle,
  state: Dict(String, #(Int, List(Int))),
  fixed_dict: Dict(String, #(Int, List(Int))),
  cur_ch: String,
  after: List(String),
) {
  let used = case state |> dict.get(cur_ch) {
    Ok(#(_, used)) -> used
    _ -> []
  }

  let allocated_others =
    state
    |> dict.filter(fn(k, _v) { !{ after |> list.contains(k) } })
    |> dict.values
    |> list.map(fn(el) {
      let #(ch, _used) = el
      ch
    })
    |> list.append(used)
    |> list.append(fixed_dict |> dict.values |> list.map(fn(v) { v.0 }))

  let allocated_others = case puzzle.no_zeros |> list.contains(cur_ch) {
    True -> allocated_others |> list.prepend(0)
    False -> allocated_others
  }

  puzzle.all_i |> set.difference(allocated_others |> set.from_list)
}

fn map_i_to_parts(
  parts: #(List(String), String),
  state: Dict(String, #(Int, List(Int))),
) {
  let str = parts.0 |> string.join(" + ") <> " == " <> parts.1

  let assert [l, r] =
    state
    |> dict.fold(str, fn(acc, ch, s) {
      let #(i, _) = s
      acc |> string.replace(ch, i |> int.to_string)
    })
    |> string.split(" == ")

  #(l |> string.split(" + "), r)
}

fn is_correct_parts(parts: #(List(String), String), col: Int) {
  let divider =
    int.power(10, col |> int.to_float) |> result.unwrap(0.0) |> float.round
  let #(l, r) = parts

  {
    {
      l
      |> list.fold(0, fn(acc, cur) {
        acc + { cur |> int.parse |> result.unwrap(0) }
      })
    }
    % divider
  }
  == { r |> int.parse |> result.unwrap(0) }
}

fn allocate_next_num(
  id: Int,
  state: Dict(String, #(Int, List(Int))),
  fixed_dict: Dict(String, #(Int, List(Int))),
  map: #(List(#(Int, String)), List(#(String, Int))),
  puzzle: Puzzle,
) {
  let #(i_ch, _) = map
  let letters_len = list.length(i_ch)

  case id {
    id if id >= 0 -> {
      let assert Ok(cur_ch) = i_ch |> list.key_find(id)

      let remains =
        get_avail_remains_ints(
          puzzle,
          state,
          fixed_dict,
          cur_ch,
          i_ch
            |> list.filter_map(fn(el) {
              case el.0 > id {
                True -> Ok(el.1)
                False -> Error(Nil)
              }
            }),
        )

      // remains number exists check.
      case remains |> set.size > 0 {
        True -> {
          let n = remains |> set.to_list |> list.first |> result.unwrap(-1)

          let new_state =
            state
            |> dict.upsert(cur_ch, fn(x) {
              case x {
                option.Some(xx) -> {
                  let #(prev, used) = xx
                  #(n, used |> list.prepend(prev))
                }
                option.None -> #(n, [])
              }
            })

          // next char next number mapping if next char exists
          case list.length(i_ch) - 1 > id {
            True -> {
              let new_state =
                i_ch
                |> list.fold(new_state, fn(acc, el) {
                  let #(i, ch) = el
                  case i > id {
                    False -> acc
                    True -> acc |> dict.delete(ch)
                  }
                })

              allocate_next_num(id + 1, new_state, fixed_dict, map, puzzle)
            }
            False -> Ok(new_state)
          }
        }
        False -> {
          case id == 0 {
            True -> Error(Nil)
            False -> allocate_next_num(id - 1, state, fixed_dict, map, puzzle)
          }
        }
      }
    }
    id if id > letters_len -> Ok(state)
    _ -> {
      Error(Nil)
    }
  }
}
// pub fn main() {
//   echo solve("HE + SEES + THE == LIGHT")
// }
