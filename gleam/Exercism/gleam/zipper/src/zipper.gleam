import gleam/option

pub type Tree(a) {
  Leaf
  Node(value: a, left: Tree(a), right: Tree(a))
}

pub opaque type Zipper(a) {
  Zipper(
    left: option.Option(Zipper(a)),
    value: option.Option(a),
    right: option.Option(Zipper(a)),
  )
}

pub fn to_zipper(tree: Tree(a)) -> Zipper(a) {
  case tree {
    Leaf -> Zipper(option.None, option.None, option.None)
    Node(v, l, r) -> {
      case l, r {
        Leaf, Leaf -> Zipper(option.None, option.Some(v), option.None)
        Leaf, _ ->
          Zipper(option.None, option.Some(v), option.Some(to_zipper(r)))
        _, Leaf ->
          Zipper(option.Some(to_zipper(l)), option.Some(v), option.None)
        _, _ ->
          Zipper(
            option.Some(to_zipper(l)),
            option.Some(v),
            option.Some(to_zipper(r)),
          )
      }
    }
  }
}

pub fn to_tree(zipper: Zipper(a)) -> Tree(a) {
  case zipper {
    Zipper(l, v, r) -> {
      case l, r {
        option.None, option.None -> {
          let assert option.Some(vv) = v
          Node(vv, Leaf, Leaf)
        }
        _, option.None -> {
          let assert option.Some(vv) = v
          let assert option.Some(ll) = l
          Node(vv, to_tree(ll), Leaf)
        }
        option.None, _ -> {
          let assert option.Some(vv) = v
          let assert option.Some(rr) = r
          Node(vv, Leaf, to_tree(rr))
        }
        _, _ -> {
          let assert option.Some(vv) = v
          let assert option.Some(ll) = l
          let assert option.Some(rr) = r
          Node(vv, to_tree(ll), to_tree(rr))
        }
      }
    }
  }
}

pub fn value(zipper: Zipper(a)) -> Result(a, Nil) {
  case zipper.value {
    option.Some(vv) -> Ok(vv)
    _ -> Error(Nil)
  }
}

pub fn up(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn left(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn right(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn set_value(zipper: Zipper(a), value: a) -> Zipper(a) {
  todo
}

pub fn set_left(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn set_right(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn main() {
  echo to_zipper(Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    ))
    |> to_tree
}
