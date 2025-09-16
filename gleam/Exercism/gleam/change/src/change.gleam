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
      todo
    }
  }
}

pub fn main() {
  echo find_fewest_coins([2, 5, 10, 20, 50], 21)
}
