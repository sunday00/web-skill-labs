import gleam/list

pub type Classification {
  Perfect
  Abundant
  Deficient
}

pub type Error {
  NonPositiveInt
}

pub fn classify(number: Int) -> Result(Classification, Error) {
  case number {
    n if n <= 0 -> Error(NonPositiveInt)
    n if n == 1 || n == 2 || n == 3 -> Ok(Deficient)
    _ -> {
      let aliquots = get_aliquot([], number, 2)
      case aliquots |> list.length {
        n if n > 0 -> {
          let assert Ok(res) = aliquots |> list.reduce(fn(acc, x) { acc + x })

          case res + 1 {
            m if m == number -> Ok(Perfect)
            m if m < number -> Ok(Deficient)
            m if m > number -> Ok(Abundant)
            _ -> Error(NonPositiveInt)
          }
        }
        _ -> {
          Ok(Deficient)
        }
      }
    }
  }
}

fn get_aliquot(a: List(Int), n: Int, cur: Int) {
  case cur {
    c if c > n / 2 -> a
    c if n % c == 0 -> {
      get_aliquot(a |> list.append([c]), n, c + 1)
    }
    _ -> {
      get_aliquot(a, n, cur + 1)
    }
  }
}
// pub fn main() {
//   classify(33_550_337)
// }
