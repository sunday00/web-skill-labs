pub fn square_root(radicand: Int) -> Int {
  case radicand {
    1 -> 1
    _ -> reducer(0, radicand)
  }
}

fn reducer(cur: Int, target: Int) {
  case { cur * cur } == target {
    True -> cur
    False -> {
      case cur >= { target / 2 } {
        True -> 0
        False -> reducer(cur + 1, target)
      }
    }
  }
}
