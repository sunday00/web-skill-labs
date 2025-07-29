import gleam/int
import gleam/list

pub type Item {
  Item(value: Int, weight: Int)
}

pub fn maximum_value(items: List(Item), maximum_weight: Int) -> Int {
  case items |> list.first {
    Error(_) -> 0
    Ok(_) -> {
      reducer(0, 0, maximum_weight, items)
    }
  }
}

fn reducer(acc: Int, acc_weight: Int, maximum_weight: Int, remains: List(Item)) {
  case list.length(remains) {
    0 -> acc
    _ -> {
      let assert Ok(cur_item) = remains |> list.first
      let new_remains = remains |> list.drop(1)
      case acc_weight + cur_item.weight {
        x if x > maximum_weight ->
          reducer(acc, acc_weight, maximum_weight, new_remains)
        _ ->
          int.max(
            reducer(
              acc + cur_item.value,
              acc_weight + cur_item.weight,
              maximum_weight,
              new_remains,
            ),
            reducer(acc, acc_weight, maximum_weight, new_remains),
          )
      }
    }
  }
}
