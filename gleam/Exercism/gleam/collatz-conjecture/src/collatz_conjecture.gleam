pub type Error {
  NonPositiveNumber
}

pub fn steps(number: Int) -> Result(Int, Error) {
  case number > 0 {
    True -> Ok(accum(0, number))
    False -> Error(NonPositiveNumber)
  }
}

fn accum(acc: Int, n: Int) {
  case n {
    1 -> acc
    _ -> {
      case n % 2 {
        0 -> accum(acc + 1, n / 2)
        _ -> accum(acc + 1, n * 3 + 1)
      }
    }
  }
}
