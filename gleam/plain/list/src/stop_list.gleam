import gleam/list

pub fn main() {
  let l = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20]

  echo l
    |> list.fold_until(0, fn(acc, cur) {
      echo cur

      case cur > 4 {
        True -> list.Stop(acc)
        False -> list.Continue(acc + cur)
      }
    })
}
