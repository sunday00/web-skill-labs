import gleam/list

pub fn rows(n: Int) -> List(List(Int)) {
  case n {
    0 -> []
    1 -> [[1]]
    2 -> [[1], [1, 1]]
    3 -> [[1], [1, 1], [1, 2, 1]]
    _ -> reducer([[1], [1, 1], [1, 2, 1]], [1, 2, 1], [1], n)
  }
}

fn reducer(acc: List(List(Int)), last: List(Int), cur: List(Int), n: Int) {
  case acc |> list.length == n {
    True -> acc
    False -> {
      case last |> list.length {
        0 -> reducer(acc |> list.append([cur]), cur, [1], n)
        1 -> {
          let new_cur = cur |> list.append([1])
          reducer(acc |> list.append([new_cur]), new_cur, [1], n)
        }
        _ -> {
          let assert [fs, se, ..] = last
          let new_last = last |> list.drop(1)

          reducer(acc, new_last, cur |> list.append([fs + se]), n)
        }
      }
    }
  }
}
