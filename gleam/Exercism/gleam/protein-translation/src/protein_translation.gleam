import gleam/list
import gleam/regexp
import gleam/string

pub fn proteins(rna: String) -> Result(List(String), Nil) {
  let assert Ok(patt) = regexp.from_string("...")

  let tmp_targets = regexp.scan(patt, rna) |> list.map(fn(el) { el.content })
  let remains = string.replace(rna, tmp_targets |> string.concat, "")

  let targets = case remains |> string.length {
    len if len > 0 -> {
      list.append(tmp_targets, [remains])
    }
    _ -> tmp_targets
  }

  let res =
    targets
    |> list.fold([], reducer)

  case res |> list.contains("ERROR") {
    True -> Error(Nil)
    False -> res |> list.filter(fn(l) { l != "STOP" }) |> Ok
  }
}

fn reducer(res: List(String), cur: String) {
  case res |> list.contains("STOP") {
    True -> {
      res
    }
    False -> {
      case cur {
        c if c == "AUG" -> res |> list.append(["Methionine"])
        c if c == "UUU" || c == "UUC" -> res |> list.append(["Phenylalanine"])
        c if c == "UUA" || c == "UUG" -> res |> list.append(["Leucine"])
        c if c == "UCU" || c == "UCC" || c == "UCA" || c == "UCG" ->
          res |> list.append(["Serine"])
        c if c == "UAU" || c == "UAC" -> res |> list.append(["Tyrosine"])
        c if c == "UGU" || c == "UGC" -> res |> list.append(["Cysteine"])
        c if c == "UGG" -> res |> list.append(["Tryptophan"])
        c if c == "UAA" || c == "UAG" || c == "UGA" ->
          res |> list.append(["STOP"])
        _ -> {
          case cur |> string.length {
            l if l == 0 -> res
            _ -> res |> list.append(["ERROR"])
          }
        }
      }
    }
  }
}
// pub fn main() {
//   echo proteins("XYZ")
// }
