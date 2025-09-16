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
    _ -> {
      let coins = coins |> list.filter(fn(el) { el < target }) |> list.reverse

      reducer([], [], coins, target, coins)

      todo
    }
  }
}

fn reducer(
  total_res: List(List(Int)),
  acc: List(Int),
  rem: List(Int),
  target: Int,
  initial_coins: List(Int),
) {
  echo #(total_res, acc, rem)
  case rem {
    [f, ..r] -> {
      let sum = acc |> list.reduce(fn(ac, cu) { ac + cu }) |> result.unwrap(0)
      case sum {
        s if s > target -> {
          reducer(
            total_res,
            acc |> list.reverse |> list.drop(1) |> list.reverse,
            r,
            target,
            initial_coins,
          )
        }
        s if s == target -> {
          todo
        }
        _ -> {
          reducer(
            total_res,
            acc |> list.append([f]),
            rem,
            target,
            initial_coins,
          )
        }
      }
    }
    [] -> {
      case acc {
        [_af, ..ar] -> {
          todo
        }
        [] -> {
          todo
        }
      }
    }
  }
}

pub fn main() {
  echo find_fewest_coins([2, 5, 10, 20, 50], 21)
}
