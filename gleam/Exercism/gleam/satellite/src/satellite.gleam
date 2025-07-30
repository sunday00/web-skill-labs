import gleam/list

pub type Tree(a) {
  Nil
  Node(value: a, left: Tree(a), right: Tree(a))
}

pub type Error {
  DifferentLengths
  DifferentItems
  NonUniqueItems
}

pub fn tree_from_traversals(
  inorder inorder: List(a),
  preorder preorder: List(a),
) -> Result(Tree(a), Error) {
  case list.length(inorder) == list.length(preorder) {
    True -> {
      case preorder {
        [] -> Ok(Nil)
        [f] -> Ok(Node(f, Nil, Nil))
        r -> {
          let is_unique =
            list.fold(r, True, fn(acc, cur) {
              acc && r |> list.filter(fn(l) { l == cur }) |> list.length == 1
            })

          let is_same =
            list.fold(r, True, fn(acc, cur) {
              acc
              && inorder |> list.filter(fn(l) { l == cur }) |> list.length == 1
            })

          case is_unique, is_same {
            True, True -> {
              let res = reducer(Nil, r)

              Ok(res)
            }
            True, False -> Error(DifferentItems)
            False, _ -> Error(NonUniqueItems)
          }
        }
      }
    }
    False -> Error(DifferentLengths)
  }
}

fn reducer(acc: Tree(a), remains: List(a)) {
  case remains {
    [] -> acc
    [f] -> Node(f, acc, Nil)
    [e1, e2, ..r] -> Node(e1, Node(e2, Nil, Nil), reducer(acc, r))
  }
}
