import gleam/int
import gleam/list
import gleam/order.{Eq, Gt, Lt}

pub type Tree {
  Nil
  Node(data: Int, left: Tree, right: Tree)
}

pub fn to_tree(data: List(Int)) -> Tree {
  data
  |> list.fold(Nil, fn(acc: Tree, i) { reducing_tree(acc, i) })
}

fn reducing_tree(acc: Tree, cur: Int) {
  case acc {
    Nil -> Node(cur, Nil, Nil)
    Node(d, l, r) -> {
      case int.compare(cur, d) {
        Lt | Eq -> Node(d, reducing_tree(l, cur), r)
        Gt -> Node(d, l, reducing_tree(r, cur))
      }
    }
  }
}

pub fn sorted_data(data: List(Int)) -> List(Int) {
  let t = to_tree(data)

  reducing_sort([], t)
}

fn reducing_sort(acc: List(Int), data: Tree) {
  case data {
    Nil -> acc
    Node(d, l, r) -> {
      case l, d, r {
        Node(_, _, _), dd, _ -> {
          reducing_sort()
        }
        Nil, d, _ -> reducing_sort(acc, d)
        _, _, Node(_, _, _) -> todo
      }
    }
  }
}

pub fn main() {
  echo to_tree([2, 1, 3, 6, 7, 5])
}
