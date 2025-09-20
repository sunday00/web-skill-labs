import gleam/bit_array
import gleam/int
import gleam/list

pub type Error {
  IncompleteSequence
}

pub fn encode(integers: List(Int)) -> BitArray {
  integers
  |> list.map(encode_single)
  |> list.fold(<<>>, bit_array.append)
}

fn encode_single(n: Int) -> BitArray {
  case n {
    0 -> <<0:8>>
    _ -> do_encode(n, [])
  }
}

fn do_encode(n: Int, acc: List(Int)) -> BitArray {
  case n {
    0 -> {
      // Convert accumulated bytes to BitArray
      acc
      |> list.map(fn(byte) { <<byte:8>> })
      |> list.fold(<<>>, bit_array.append)
    }
    _ -> {
      let byte = int.bitwise_and(n, 0x7F)
      let next_bytes = case acc {
        [] -> [byte]
        _ -> [int.bitwise_or(byte, 0x80), ..acc]
      }
      do_encode(int.bitwise_shift_right(n, 7), next_bytes)
    }
  }
}

pub fn decode(bytes: BitArray) -> Result(List(Int), Error) {
  do_decode(bytes, 0, [], [])
}

fn do_decode(
  bytes: BitArray,
  current: Int,
  numbers: List(Int),
  pending: List(Int),
) -> Result(List(Int), Error) {
  case bytes {
    <<>> ->
      case pending {
        [] -> Ok(list.reverse(numbers))
        _ -> Error(IncompleteSequence)
      }
    <<byte:8, rest:bits>> -> {
      let value = int.bitwise_and(byte, 0x7F)
      let new_current =
        int.bitwise_or(int.bitwise_shift_left(current, 7), value)

      case int.bitwise_and(byte, 0x80) {
        0 -> {
          // End of current number
          let new_numbers = case pending {
            [] -> [new_current, ..numbers]
            _ -> [new_current, ..numbers]
          }
          do_decode(rest, 0, new_numbers, [])
        }
        _ -> {
          // Continue reading number
          do_decode(rest, new_current, numbers, [byte, ..pending])
        }
      }
    }
    _ -> Error(IncompleteSequence)
  }
}
