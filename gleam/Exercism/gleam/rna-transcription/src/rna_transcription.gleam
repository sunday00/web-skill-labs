import gleam/regex
import gleam/result
import gleam/string

fn to_each(l: String) -> String {
  case l {
    "G" -> "C"
    "C" -> "G"
    "T" -> "A"
    "A" -> "U"
    _ -> ""
  }
}

fn reducer(acc: String, remains: String) -> String {
  case remains {
    "" -> acc
    _ -> {
      let f = remains |> string.first |> result.unwrap("")
      let r = remains |> string.drop_left(1)
      reducer(acc <> to_each(f), r)
    }
  }
}

pub fn to_rna(dna: String) -> Result(String, Nil) {
  let assert Ok(r) = regex.from_string("[^GCTA]")
  case dna |> regex.check(r, _) {
    True -> Error(Nil)
    False -> dna |> reducer("", _) |> Ok
  }
}
