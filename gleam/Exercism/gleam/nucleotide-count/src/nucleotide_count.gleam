import gleam/dict.{type Dict}
import gleam/option
import gleam/string

pub fn nucleotide_count(dna: String) -> Result(Dict(String, Int), Nil) {
  reducer([#("A", 0), #("C", 0), #("G", 0), #("T", 0)] |> dict.from_list, dna)
}

fn reducer(counts: Dict(String, Int), remains: String) {
  case remains |> string.length {
    n if n > 0 -> {
      let assert Ok(f) = remains |> string.first
      let new_remains = remains |> string.drop_start(1)

      case f {
        f if f == "A" || f == "C" || f == "G" || f == "T" -> {
          reducer(
            counts
              |> dict.upsert(f, fn(o) {
                case o {
                  option.Some(i) -> i + 1
                  option.None -> 0
                }
              }),
            new_remains,
          )
        }
        _ -> Error(Nil)
      }
    }
    _ -> {
      Ok(counts)
    }
  }
}
