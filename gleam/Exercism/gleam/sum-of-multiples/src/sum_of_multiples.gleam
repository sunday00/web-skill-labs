import gleam/list
import gleam/set

fn multiples(acc: List(Int), cur: Int, add: Int, limit: Int) {
  case cur >= limit {
    True -> acc |> set.from_list
    False -> {
      multiples([cur, ..acc], cur + add, add, limit)
    }
  }
}

pub fn sum(factors factors: List(Int), limit limit: Int) -> Int {
  factors
  |> list.fold(set.from_list([]), fn(acc, cur) {
    case cur == 0 {
      True -> acc |> set.union(set.from_list([0]))
      False -> acc |> set.union(multiples([], cur, cur, limit))
    }
  })
  |> set.fold(0, fn(acc, cur) { acc + cur })
}
// pub fn main() {
//   echo sum([3, 0], 4)
// }
