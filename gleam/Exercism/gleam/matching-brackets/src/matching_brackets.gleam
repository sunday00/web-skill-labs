import gleam/list
import gleam/regexp
import gleam/result

pub fn is_paired(value: String) -> Bool {
  let a = regexp.scan(patt(), value)

  a
  |> list.map(reduceer)
  |> list.reduce(fn(acc, l) { acc && l })
  |> result.unwrap(False)
}

fn reduceer(v: regexp.Match) {
  let inner = regexp.scan(patt(), v.content)

  echo inner

  case inner |> list.length() {
    n if n > 0 -> {
      inner
      |> list.map(reduceer)
      |> list.reduce(fn(acc, l) { acc && l })
      |> result.unwrap(False)
    }
    _ -> {
      !regexp.check(single_patt(), v.content)
    }
  }
}

fn patt() {
  let assert Ok(patt) =
    regexp.from_string(
      "(?<=\\[)(.+)(?=\\])|(?<=\\{)(.+)(?=\\})|(?<=\\()(.+)(?=\\))",
    )

  patt
}

fn single_patt() {
  let assert Ok(patt) = regexp.from_string("\\[|\\]|\\{|\\}|\\(|\\)")

  patt
}

pub fn main() {
  echo is_paired("([{}({}[])])")
}
