import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/result

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

pub fn decode(string: BitArray) -> Result(List(Int), Error) {
  todo
}

pub fn main() {
  echo encode([0x80])
  echo <<129, 0>> == <<0x81, 0x0>>
}
