import gleam/list

pub fn factors(value: Int) -> List(Int) {
  case value {
    v if v == 1 -> []
    _ -> {
      reducer([], 2, value)
    }
  }
}

fn reducer(acc: List(Int), cur: Int, remains: Int) {
  case remains {
    r if r % cur == 0 -> reducer(acc |> list.append([cur]), cur, remains / cur)
    r if r > cur -> reducer(acc, cur + 1, remains)
    r if r == cur -> acc |> list.append([cur])
    _ -> acc
    // should not reach?
  }
}
