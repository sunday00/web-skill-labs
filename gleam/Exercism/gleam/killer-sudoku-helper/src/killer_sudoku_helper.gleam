import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result

pub fn combinations(
  size size: Int,
  sum sum: Int,
  exclude exclude: List(Int),
) -> List(List(Int)) {
  case size {
    1 -> [[sum]]
    _ -> {
      reducer(
        [],
        [],
        [1, 2, 3, 4, 5, 6, 7, 8, 9]
          |> list.filter(fn(n) { !list.contains(exclude, n) }),
        size,
        sum,
      )
    }
  }
}

fn reducer(
  acc: List(List(Int)),
  cur: List(Int),
  remains: List(Int),
  size: Int,
  sum: Int,
) {
  case list.length(remains) {
    0 -> acc
    _ -> {
      case
        list.reduce(cur, fn(ac, cu) { ac + cu }) |> result.unwrap(0) == sum,
        list.length(cur),
        list.length(remains)
      {
        True, len, _ if len == size -> {
          reducer(acc |> list.append([cur]), [], remains, size, sum)
        }

        False, len, _ if len == size - 1 -> {
          let assert Ok(t) = remains |> list.first
          let new_remains = remains |> list.drop(1)

          case
            { list.reduce(cur, fn(ac, cu) { ac + cu }) |> result.unwrap(0) } + t
          {
            x if x == sum -> {
              reducer(
                acc |> list.append([cur |> list.append([t])]),
                [],
                new_remains |> list.sort(int.compare),
                size,
                sum,
              )
            }
            x if x > sum -> {
              reducer(acc, [], new_remains |> list.sort(int.compare), size, sum)
            }
            _ -> {
              reducer(acc, cur, new_remains |> list.append([t]), size, sum)
            }
          }
        }

        _, _, len if len <= size -> {
          case list.length(remains) {
            l if l < size -> acc
            _ -> {
              case
                list.reduce(remains, fn(ac, cu) { ac + cu }) |> result.unwrap(0)
                == sum
              {
                True -> {
                  list.append(acc, [remains])
                }
                False -> acc
              }
            }
          }
        }

        _, _, _ -> {
          let assert Ok(t) = remains |> list.first
          let new_remains = remains |> list.drop(1)

          case
            { list.reduce(cur, fn(ac, cu) { ac + cu }) |> result.unwrap(0) } + t
          {
            x if x >= sum -> {
              acc
            }
            _ -> {
              reducer(
                acc,
                cur |> list.append([t]),
                new_remains |> list.sort(int.compare),
                size,
                sum,
              )
            }
          }
        }
      }
    }
  }
}
