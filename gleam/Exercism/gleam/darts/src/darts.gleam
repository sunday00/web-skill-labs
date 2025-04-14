import gleam/float
import gleam/result

pub fn score(x: Float, y: Float) -> Int {
  let assert Ok(pow_x) = float.power(x |> float.absolute_value, 2.0)
  let assert Ok(pow_y) = float.power(y |> float.absolute_value, 2.0)
  let assert Ok(r) = { pow_x +. pow_y } |> float.square_root |> result.nil_error

  case r {
    r if r <=. 1.0 -> 10
    r if r <=. 5.0 -> 5
    r if r <=. 10.0 -> 1
    _ -> 0
  }
}
