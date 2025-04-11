import gleam/list

// import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub fn new_collection(card: String) -> Set(String) {
  [card] |> set.from_list
}

pub fn add_card(collection: Set(String), card: String) -> #(Bool, Set(String)) {
  #(set.contains(collection, card), set.insert(collection, card))
}

pub fn trade_card(
  my_card: String,
  their_card: String,
  collection: Set(String),
) -> #(Bool, Set(String)) {
  let success =
    set.contains(collection, my_card) && !set.contains(collection, their_card)
  let expect =
    set.from_list([their_card, ..set.to_list(set.delete(collection, my_card))])

  #(success, expect)
}

// fn intersect_collection(acc: Set(String), remains: List(Set(String))) {
//   case remains {
//     [] -> acc
//     [f, ..rest] -> intersect_collection(set.intersection(acc, f), rest)
//   }
// }

pub fn boring_cards(collections: List(Set(String))) -> List(String) {
  // case collections {
  //   [] -> []
  //   [f, ..rest] ->
  //     intersect_collection(f, rest)
  //     |> set.to_list
  //     |> list.sort(fn(a, b) { string.compare(a, b) })
  // }
  list.reduce(collections, set.intersection)
  |> result.unwrap(set.new())
  |> set.to_list
}

// fn merge_collection(acc: Set(String), remains: List(Set(String))) {
//   case remains {
//     [] -> acc
//     [f, ..rest] -> merge_collection(set.union(acc, f), rest)
//   }
// }

pub fn total_cards(collections: List(Set(String))) -> Int {
  // case collections {
  //   [] -> 0
  //   [f, ..rest] -> merge_collection(f, rest) |> set.size
  // }

  list.fold(collections, set.new(), set.union) |> set.size
}

pub fn shiny_cards(collection: Set(String)) -> Set(String) {
  collection |> set.filter(fn(c) { c |> string.starts_with("Shiny ") })
}
