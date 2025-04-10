import gleam/bit_array
import gleam/list

pub type Nucleotide {
  Adenine
  Cytosine
  Guanine
  Thymine
}

pub fn encode_nucleotide(nucleotide: Nucleotide) -> Int {
  case nucleotide {
    Adenine -> 0b00
    Cytosine -> 0b01
    Guanine -> 0b10
    Thymine -> 0b11
  }
}

pub fn decode_nucleotide(nucleotide: Int) -> Result(Nucleotide, Nil) {
  case nucleotide {
    0 -> Ok(Adenine)
    1 -> Ok(Cytosine)
    2 -> Ok(Guanine)
    3 -> Ok(Thymine)
    _ -> Error(Nil)
  }
}

fn reduce_merging(acc: BitArray, remains: List(Nucleotide)) {
  case remains {
    [] -> acc
    [f, ..r] -> {
      let b = encode_nucleotide(f)
      reduce_merging(<<acc:bits, b:size(2)>>, r)
    }
  }
}

pub fn encode(dna: List(Nucleotide)) -> BitArray {
  reduce_merging(<<>>, dna)
}

fn reduce_decoding(acc: List(Nucleotide), remains: BitArray) {
  case remains {
    <<>> -> Ok(acc |> list.reverse)
    <<d:2, rest:bits>> -> {
      let nd = decode_nucleotide(d)
      case nd {
        Ok(nn) -> reduce_decoding([nn, ..acc], rest)
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

pub fn decode(dna: BitArray) -> Result(List(Nucleotide), Nil) {
  reduce_decoding([], dna)
}
// pub fn main() {
//   echo encode([Thymine, Guanine, Cytosine, Adenine])
// }
