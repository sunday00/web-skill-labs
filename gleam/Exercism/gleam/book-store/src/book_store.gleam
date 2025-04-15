import gleam/dict
import gleam/float
import gleam/int
import gleam/list

pub fn lowest_price(books: List(Int)) -> Float {
  books
  |> frequencies
  |> calc_price(0.0)
}

pub fn frequencies(list: List(a)) -> List(Int) {
  list
  |> list.group(fn(a) { a })
  |> dict.fold([], fn(l, _k, v) { [list.length(v), ..l] })
}

pub fn calc_price(frequencies: List(Int), acc: Float) -> Float {
  case list.sort(frequencies, int.compare) {
    [a] | [0, 0, 0, 0, a] -> acc +. int.to_float(a) *. 800.0
    [a, b] | [0, 0, 0, a, b] ->
      calc_price([0, 0, 0, a - 1, b - 1], acc +. 2.0 *. 760.0)
    [a, b, c] | [0, 0, a, b, c] ->
      calc_price([0, 0, a - 1, b - 1, c - 1], acc +. 3.0 *. 720.0)
    [a, b, c, d] | [0, a, b, c, d] ->
      calc_price([0, a - 1, b - 1, c - 1, d - 1], acc +. 4.0 *. 640.0)
    [a, b, c, d, e] ->
      float.min(
        calc_price([a - 1, b - 1, c - 1, d - 1, e - 1], acc +. 5.0 *. 600.0),
        calc_price([a, b - 1, c - 1, d - 1, e - 1], acc +. 4.0 *. 640.0),
      )
    _ -> acc
  }
}
