import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/set.{type Set}
import gleam/string

type Puzzle {
  Puzzle(puzzle: String, left: List(String), right: String, all_i: Set(Int))
}

type Char {
  Char(txt: String, cur: Int, used: Set(Int))
}

pub fn solve(puzzle: String) -> Result(Dict(String, Int), Nil) {
  let assert [left, right] = puzzle |> string.split(" == ")
  let puzzle =
    Puzzle(
      puzzle,
      left |> string.split(" + "),
      right,
      list.range(0, 9) |> set.from_list,
    )

  echo resolve(puzzle)

  Ok(dict.new())
}

fn resolve(puzzle: Puzzle) {
  let all_letters =
    puzzle.left
    |> list.map(fn(l) { l |> string.to_graphemes })
    |> list.flatten
    |> list.append(puzzle.right |> string.to_graphemes)
    |> list.unique
    |> list.sort(string.compare)

  let state =
    do_with_nth_tails(puzzle, #([], ""), dict.new(), dict.new(), [], 1, 0)

  echo state
}

fn do_with_nth_tails(
  puzzle: Puzzle,
  nth_tail_str: #(List(String), String),
  cur_sate: Dict(String, Int),
  used_map: Dict(String, List(Int)),
  fixed_char: List(String),
  col: Int,
  carry: Int,
) {
  let #(left_tails, right_tails) = nth_tail_str

  case left_tails |> list.length == 0 && right_tails == "" {
    True -> {
      // no initiated - ignite
      do_with_nth_tails(
        puzzle,
        extract_nth_tails(puzzle, col),
        cur_sate,
        used_map,
        fixed_char,
        col,
        carry,
      )
    }
    False -> {
      // ignited
      let splits =
        left_tails
        |> list.append([right_tails])
        |> list.map(fn(s) { s |> string.to_graphemes })
        |> list.flatten

      let rests =
        splits |> set.from_list |> set.difference(set.from_list(fixed_char))

      // TODO: save last success parts.
      // TODO: check all failed, back to last set.

      let #(map, used_map) =
        get_next_mapping(
          cur_sate,
          rests |> set.to_list,
          puzzle,
          rests |> set.to_list,
          used_map,
        )

      let tmp_left_joins = left_tails |> string.join("|")

      let replaced_left_ints =
        map
        |> dict.fold(tmp_left_joins, fn(acc, ch, i) {
          acc |> string.replace(ch, i |> int.to_string)
        })
        |> string.split("|")
        |> list.map(fn(l) { l |> int.parse |> result.unwrap(0) })

      let replaced_right_ints =
        map
        |> dict.fold(right_tails, fn(acc, ch, i) {
          acc |> string.replace(ch, i |> int.to_string)
        })
        |> int.parse
        |> result.unwrap(0)

      let left_ints =
        replaced_left_ints |> list.fold(0, fn(acc, cur) { acc + cur })

      let div = list.repeat(10, col) |> list.fold(1, fn(a, c) { a * c })

      echo #(map, left_tails, right_tails)
      process.sleep(10)

      case left_ints % div + carry == replaced_right_ints {
        True -> {
          // word last calculated success. more letters add on word cutting.
          echo map
          process.sleep(1000)

          do_with_nth_tails(
            puzzle,
            #([], ""),
            map,
            used_map,
            map |> dict.keys,
            col + 1,
            { left_ints / div } * div,
          )
        }
        False -> {
          // do next number use mapping
          do_with_nth_tails(
            puzzle,
            nth_tail_str,
            map,
            used_map,
            fixed_char,
            col,
            carry,
          )
        }
      }
    }
  }
}

fn extract_nth_tails(puzzle: Puzzle, col: Int) {
  let left_tails =
    puzzle.left |> list.map(fn(s) { s |> string.slice(col * -1, col) })
  let right_tails = puzzle.right |> string.slice(col * -1, col)

  #(left_tails, right_tails)
}

fn get_next_mapping(
  acc: Dict(String, Int),
  txt: List(String),
  puzzle: Puzzle,
  fixed_text: List(String),
  used_map: Dict(String, List(Int)),
) {
  case txt {
    // has more texts
    [f, ..r] -> {
      case
        r |> list.length == 0 || acc |> dict.get(f) |> result.unwrap(-1) < 0
      {
        // initiallize || last current letter.
        True -> {
          let prev_used = case acc |> dict.get(f) {
            Ok(v) ->
              used_map |> dict.get(f) |> result.unwrap([]) |> list.append([v])
            _ -> used_map |> dict.get(f) |> result.unwrap([])
          }

          let current_other_letter_used = acc |> dict.values
          let canbe =
            puzzle.all_i
            |> set.difference(prev_used |> set.from_list)
            |> set.difference(current_other_letter_used |> set.from_list)
            |> set.to_list
            |> list.first

          case canbe {
            // can replace next int allocate
            Ok(i) -> {
              // todo : no zero check

              case acc |> dict.get(f) {
                // used letter 
                Ok(v) -> {
                  let re_rest =
                    acc
                    |> dict.to_list
                    |> list.filter_map(fn(l) {
                      let #(ch, i) = l
                      case i == -1 {
                        True -> Ok(ch)
                        False -> Error(Nil)
                      }
                    })

                  case re_rest |> list.length == 0 {
                    True ->
                      get_next_mapping(
                        acc |> dict.upsert(f, fn(_) { i }),
                        [],
                        puzzle,
                        fixed_text,
                        used_map
                          |> dict.upsert(f, fn(p) {
                            case p {
                              option.Some(l) -> l |> list.prepend(v)
                              _ -> [v]
                            }
                          }),
                      )
                    False ->
                      get_next_mapping(
                        acc |> dict.upsert(f, fn(_) { i }),
                        [f, ..re_rest],
                        puzzle,
                        fixed_text,
                        used_map
                          |> dict.upsert(f, fn(p) {
                            case p {
                              option.Some(l) -> l |> list.prepend(v)
                              _ -> [v]
                            }
                          }),
                      )
                  }
                }
                // no used letter prev
                _ -> {
                  get_next_mapping(
                    acc |> dict.upsert(f, fn(_) { i }),
                    r,
                    puzzle,
                    fixed_text,
                    used_map,
                  )
                }
              }
            }
            // last used number. truncate and re allocate on foward letter.
            _ -> {
              let new_txt =
                fixed_text
                |> list.fold_until([], fn(a, c) {
                  case c == f {
                    True -> list.Stop(a)
                    False -> list.Continue(list.append(a, [c]))
                  }
                })

              // TODO: failed with all map all used.

              get_next_mapping(
                acc |> dict.upsert(f, fn(_) { -1 }),
                new_txt,
                puzzle,
                fixed_text,
                used_map |> dict.upsert(f, fn(_) { [] }),
              )
            }
          }
        }
        False -> {
          // skip already letter
          get_next_mapping(acc, r, puzzle, fixed_text, used_map)
        }
      }
    }
    [] -> #(acc, used_map)
    // done with this loop
  }
}

pub fn main() {
  echo solve("SEND + MORE == MONEY")
}
