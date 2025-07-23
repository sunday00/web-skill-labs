import gleam/int

pub fn prime(number: Int) -> Result(Int, Nil) {
  reducer(2, 2, number, 1)
}

fn reducer(acc: Int, divider: Int, target_idx: Int, cur_idx: Int) {
  // echo acc |> int.to_string()
  // <> "/"
  // <> divider |> int.to_string()
  // <> "/"
  // <> target_idx |> int.to_string()
  // <> "/"
  // <> cur_idx |> int.to_string()

  case cur_idx {
    n if n == target_idx -> {
      case acc {
        a if a % divider == 0 -> {
          case divider {
            d if d == a / 2 || d < a / 2 ->
              reducer(acc + 1, 2, target_idx, cur_idx)
            d if d == a -> Ok(acc)
            _ -> Error(Nil)
          }
        }
        a if a / 2 < divider -> {
          Ok(acc)
        }
        _ -> reducer(acc, divider + 1, target_idx, cur_idx)
      }
    }
    n if n > target_idx -> Error(Nil)
    n if n < target_idx -> {
      case acc {
        a if a % divider == 0 -> {
          case divider {
            d if d == a / 2 || d < a / 2 ->
              reducer(acc + 1, 2, target_idx, cur_idx)
            d if d == a -> reducer(acc + 1, 2, target_idx, cur_idx + 1)
            _ -> Error(Nil)
          }
        }
        a if a / 2 < divider -> {
          reducer(acc + 1, 2, target_idx, cur_idx + 1)
        }
        _ -> reducer(acc, divider + 1, target_idx, cur_idx)
      }
    }
    _ -> Error(Nil)
  }
}
// pub fn main() {
//   // echo prime(10_001)
//   // echo prime(3)
//   echo prime(0)
// }

// ref
// pub fn prime(number: Int) -> Result(Int, Nil) {
//   case number {
//     0 -> Error(Nil)
//     1 -> Ok(2)
//     _ -> Ok(get_nth(number, 2, 3))
//   }
// }

// fn get_nth(nth: Int, counter: Int, current: Int) -> Int {
//   case counter == nth, is_prime(current, 3) {
//     True, True -> current
//     _, True -> get_nth(nth, counter + 1, current + 2)
//     _, False -> get_nth(nth, counter, current + 2)
//   }
// }

// fn is_prime(n: Int, k: Int) -> Bool {
//   k * k > n || n % k != 0 && is_prime(n, k + 2)
// }
