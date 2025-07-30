import gleam/int
import gleam/list
import gleam/result

pub type Position {
  Position(row: Int, column: Int)
}

pub fn saddle_points(matrix: List(List(Int))) -> List(Position) {
  case matrix |> list.first {
    Ok(el) -> {
      case el {
        [] -> []
        [_, ..] -> {
          let index_matrix =
            matrix
            |> list.index_map(fn(row, i) {
              #(i + 1, row |> list.index_map(fn(c, y) { #(y + 1, c) }))
            })

          index_matrix
          |> list.fold([], fn(acc, cur) {
            let #(i, n) = cur

            let assert Ok(#(_, v)) =
              list.max(n, fn(a, b) {
                let #(_, av) = a
                let #(_, bv) = b

                int.compare(av, bv)
              })

            let all_max_in_rows =
              n
              |> list.filter(fn(l) {
                let #(_, lv) = l
                lv == v
              })

            all_max_in_rows
            |> list.fold(acc, fn(acc, cu) {
              let #(ci, n) = cu
              let all_v_in_same_col =
                index_matrix
                |> list.map(fn(l) {
                  let #(_, ll) = l
                  ll |> list.key_find(ci) |> result.unwrap(-1)
                })

              let all_min_v = case all_v_in_same_col {
                [] -> {
                  -1
                  // not reach
                }
                [f] -> f
                [f, ..r] -> list.fold(r, f, fn(a, c) { int.min(a, c) })
              }

              case n == all_min_v {
                True -> acc |> list.append([Position(i, ci)])
                False -> acc
              }
            })
          })
        }
      }
    }
    Error(_) -> []
  }
}
