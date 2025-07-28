import gleam/list

pub type NestedList(a) {
  Null
  Value(a)
  List(List(NestedList(a)))
}

pub fn flatten(nested_list: NestedList(a)) -> List(a) {
  case nested_list {
    Null -> []
    Value(v) -> [v]
    List(l) -> {
      l |> list.fold([], reducer)
    }
  }
}

fn reducer(acc, cur) {
  case cur {
    Null -> acc
    Value(a) -> acc |> list.append([a])
    List(l) -> {
      l |> list.fold(acc, reducer)
    }
  }
}
// pub fn main() {
//   echo flatten(
//     List([
//       Value(1),
//       List([Value(2), Value(3), Value(4), Value(5), Value(6), Value(7)]),
//       Value(8),
//     ]),
//   )
// }
