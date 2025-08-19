import gleam/list

pub fn primes_up_to(upper_bound: Int) -> List(Int) {
  case upper_bound {
    n if n <= 1 -> []
    n if n == 2 -> [2]
    n if n < 5 -> [2, 3]
    n if n < 7 -> [2, 3, 5]
    n if n <= 10 -> [2, 3, 5, 7]
    _ -> reducer([2, 3, 5, 7], 11, upper_bound)
  }
}

fn reducer(acc: List(Int), cur: Int, fin: Int) {
  case cur == fin, cur % 2 == 0 {
    True, True -> acc
    False, True -> reducer(acc, cur + 1, fin)
    True, False -> {
      case is_prime(cur, 3) {
        True -> acc |> list.append([cur])
        False -> acc
      }
    }
    False, False -> {
      case is_prime(cur, 3) {
        True -> reducer(acc |> list.append([cur]), cur + 1, fin)
        False -> reducer(acc, cur + 1, fin)
      }
    }
  }
}

fn is_prime(t: Int, cur: Int) {
  case t < cur * 2 {
    True -> True
    False -> {
      case t % cur == 0 {
        True -> False
        False -> is_prime(t, cur + 1)
      }
    }
  }
}
