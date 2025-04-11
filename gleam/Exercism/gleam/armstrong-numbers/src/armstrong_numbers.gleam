import gleam/float
import gleam/int
import gleam/list
import gleam/result

// pub fn is_armstrong_number(number: Int) -> Bool {
//   let assert Ok(digits) = number |> int.digits(10)
//   let count = list.length(digits) |> int.to_float

//   let r =
//     digits
//     |> list.fold(0, fn(acc, x) {
//       let acc_f = acc |> int.to_float
//       let assert Ok(pow) = int.power(x, of: count)

//       { acc_f +. pow } |> float.round
//     })

//   echo r

//   r == number
// }

pub fn is_armstrong_number(number: Int) -> Bool {
  let assert Ok(digits) = number |> int.digits(10)
  let count = list.length(digits) |> int.to_float

  let r =
    digits
    |> list.map(fn(x) {
      let k = int.power(x, of: count) |> result.unwrap(0.0) |> float.round
      let j = int_power(x, count |> float.round)

      // echo { k |> int.to_string }
      // <> "|"
      // <> { x |> int.to_string }
      // <> "|"
      // <> { count |> float.to_string }
      // echo { j |> int.to_string }
      // <> "|"
      // <> { x |> int.to_string }
      // <> "|"
      // <> { count |> float.to_string }

      j
    })
    |> int.sum

  r == number
}

// pub fn is_armstrong_number(number: Int) -> Bool {
//   let assert Ok(digits) = int.digits(number, 10)
//   let digits_count = list.length(digits)
//   let sum_of_powers =
//     digits
//     |> list.map(int_power(_, of: digits_count))
//     |> int.sum

//   echo sum_of_powers

//   sum_of_powers == number
// }

fn int_power(n: Int, power: Int) -> Int {
  do_int_power(n, power, 1)
}

fn do_int_power(n: Int, power: Int, acc: Int) -> Int {
  case power {
    0 -> acc
    _ -> do_int_power(n, power - 1, acc * n)
  }
}
// pub fn main() {
//   echo is_armstrong_number(186_709_961_001_538_790_100_634_132_976_990)
// }
