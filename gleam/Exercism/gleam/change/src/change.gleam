import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result

pub type Error {
  ImpossibleTarget
}

type StartInfo {
  StartInfo(target: Int, coins: List(Int))
}

pub fn find_fewest_coins(
  coins: List(Int),
  target: Int,
) -> Result(List(Int), Error) {
  // echo #(coins, target)

  case target {
    0 -> Ok([])
    t if t < 0 -> Error(ImpossibleTarget)
    _ -> {
      case coins |> list.contains(target) {
        True -> Ok([target])
        False -> {
          let coins =
            coins |> list.filter(fn(el) { el < target }) |> list.reverse

          case coins |> list.length == 0 {
            True -> Error(ImpossibleTarget)
            False -> {
              let start_info = StartInfo(target, coins)

              let res = reducer([], [], coins, start_info)

              case res |> list.length > 0 {
                True -> Ok(res |> list.reverse)
                False -> Error(ImpossibleTarget)
              }
            }
          }
        }
      }
    }
  }
}

fn reducer(
  total_res: List(List(Int)),
  acc: List(Int),
  rem: List(Int),
  s_info: StartInfo,
) {
  // echo #(total_res, acc, rem)
  // process.sleep(100)

  case acc, rem {
    [], [] -> {
      case total_res |> list.length > 0 {
        True -> {
          total_res
          |> list.fold(total_res |> list.first |> result.unwrap([]), fn(ac, el) {
            case list.length(ac) > list.length(el) {
              True -> el
              False -> ac
            }
          })
        }
        False -> []
      }
    }
    _, [1] -> {
      let min_candidate =
        s_info.coins
        |> list.filter(fn(el) { el != 1 })
        |> list.fold(
          s_info.coins
            |> list.first
            |> result.unwrap(0),
          fn(acc, el) { int.min(acc, el) },
        )

      case acc |> list.count(fn(el) { el == 1 }) >= min_candidate {
        True -> {
          // total_res
          // |> list.fold(total_res |> list.first |> result.unwrap([]), fn(ac, el) {
          //   case list.length(ac) > list.length(el) {
          //     True -> el
          //     False -> ac
          //   }
          // })

          let new_rem =
            s_info.coins
            |> list.filter(fn(el) {
              el < acc |> list.first() |> result.unwrap(0)
            })

          reducer(total_res, [], new_rem, s_info)
        }
        False -> reducer(total_res, acc |> list.append([1]), rem, s_info)
      }
    }
    _, _ -> {
      case rem {
        [f, ..r] -> {
          let sum =
            acc |> list.reduce(fn(ac, cu) { ac + cu }) |> result.unwrap(0)
          case sum {
            s if s > s_info.target -> {
              reducer(
                total_res,
                acc |> list.reverse |> list.drop(1) |> list.reverse,
                r,
                s_info,
              )
            }
            s if s == s_info.target -> {
              reducer(
                total_res |> list.append([acc]),
                acc
                  |> list.filter(fn(el) {
                    el != acc |> list.last |> result.unwrap(0)
                  })
                  |> list.reverse
                  |> list.drop(1)
                  |> list.reverse,
                rem |> list.drop(1),
                s_info,
              )
            }
            _ -> {
              reducer(total_res, acc |> list.append([f]), rem, s_info)
            }
          }
        }
        [] -> {
          case acc {
            _ -> {
              let new_acc = acc |> list.reverse |> list.drop(1) |> list.reverse
              let restart_coins =
                s_info.coins
                |> list.filter(fn(el) {
                  el < acc |> list.last |> result.unwrap(0)
                })

              reducer(total_res, new_acc, restart_coins, s_info)
            }
            [] -> {
              todo
            }
          }
        }
      }
    }
  }
}

pub fn main() {
  echo find_fewest_coins([1, 5, 10, 21, 25], 63)
}
