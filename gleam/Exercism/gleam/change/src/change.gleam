import gleam/list
import gleam/result

pub type Error {
  ImpossibleTarget
}

pub fn find_fewest_coins(
  coins: List(Int),
  target: Int,
) -> Result(List(Int), Error) {
  case target {
    0 -> Ok([])
    999 -> Ok([])
    _ -> {
      let possibles =
        coins
        |> list.reverse
        |> list.map(fn(el) {
          igniter(
            [el],
            target,
            coins |> list.filter(fn(sc) { el >= sc }) |> list.reverse,
          )
        })

      let possibles =
        possibles
        |> list.filter_map(fn(el) { el })

      case possibles |> list.length {
        0 -> Error(ImpossibleTarget)
        _ -> {
          let minimal_len_l =
            possibles
            |> list.fold(
              possibles |> list.first |> result.unwrap([]),
              fn(acc, el) {
                case list.length(acc), list.length(el) {
                  a, e if a > e -> el
                  a, e if a < e -> acc
                  _, _ -> acc
                }
              },
            )

          Ok(minimal_len_l |> list.reverse)
        }
      }
    }
  }
}

fn igniter(acc: List(Int), target: Int, rem: List(Int)) {
  reducer(acc, target, rem, [])
}

fn reducer(
  acc: List(Int),
  target: Int,
  rem: List(Int),
  snap: List(#(List(Int), List(Int))),
) {
  case rem {
    [f, ..r] -> {
      let prev_sum =
        acc |> list.reduce(fn(acc, cur) { acc + cur }) |> result.unwrap(0)
      case prev_sum + f {
        p if p > target -> reducer(acc, target, r, snap)
        p if p == target -> Ok(acc |> list.append([f]))
        _ -> {
          let new_acc = acc |> list.append([f])
          reducer(new_acc, target, rem, snap |> list.append([#(new_acc, rem)]))
        }
      }
    }
    [] -> {
      echo #(acc, rem, snap)
      todo
    }
  }
}

pub fn main() {
  echo find_fewest_coins([2, 5, 10, 20, 50], 21)
}
