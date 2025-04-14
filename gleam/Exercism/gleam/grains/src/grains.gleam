// import gleam/float
import gleam/int

// import gleam/list

pub type Error {
  InvalidSquare
}

pub fn square(square: Int) -> Result(Int, Error) {
  // case square {
  //   n if n > 0 && n <= 64 -> {
  //     let assert Ok(f) = int.power(2, square - 1 |> int.to_float)
  //     Ok(float.truncate(f))
  //   }
  //   _ -> Error(InvalidSquare)
  // }
  case square > 0 && square <= 64 {
    True -> {
      Ok(int.bitwise_shift_left(1, square - 1))
    }
    False -> Error(InvalidSquare)
  }
}

pub fn total() -> Int {
  // list.range(1, 64)
  // |> list.fold(0, fn(acc, cur) {
  //   let assert Ok(c) = square(cur)
  //   acc + c
  // })
  int.bitwise_shift_left(1, 64) - 1
}
