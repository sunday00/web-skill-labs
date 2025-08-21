import gleam/dict.{type Dict}
import gleam/option
import gleam/result

pub type Tree(a) {
  Leaf
  Node(value: a, left: Tree(a), right: Tree(a))
}

pub opaque type Zipper(a) {
  Zipper(
    map: Dict(a, #(option.Option(a), option.Option(a), option.Option(a))),
    cur: option.Option(a),
    top: option.Option(a),
  )
}

pub fn to_zipper(tree: Tree(a)) -> Zipper(a) {
  case tree {
    Leaf -> Zipper(dict.new(), option.None, option.None)
    Node(v, _, _) -> {
      let map = make_zipper_map(dict.new(), tree, option.None)
      Zipper(map, option.Some(v), option.Some(v))
    }
  }
}

fn make_zipper_map(
  acc: Dict(a, #(option.Option(a), option.Option(a), option.Option(a))),
  cur: Tree(a),
  parent: option.Option(a),
) {
  case cur {
    Leaf -> acc
    Node(v, l, r) -> {
      case l, r {
        Leaf, Leaf -> acc |> dict.insert(v, #(option.None, option.None, parent))
        Leaf, Node(rv, _, _) -> {
          let acc =
            acc |> dict.insert(v, #(option.None, option.Some(rv), parent))

          make_zipper_map(acc, r, option.Some(v))
        }
        Node(lv, _, _), Leaf -> {
          let acc =
            acc |> dict.insert(v, #(option.Some(lv), option.None, parent))
          make_zipper_map(acc, l, option.Some(v))
        }
        Node(lv, _, _), Node(rv, _, _) -> {
          let acc =
            acc |> dict.insert(v, #(option.Some(lv), option.Some(rv), parent))
          make_zipper_map(acc, l, option.Some(v))
          |> make_zipper_map(r, option.Some(v))
        }
      }
    }
  }
}

pub fn to_tree(zipper: Zipper(a)) -> Tree(a) {
  case zipper.top {
    option.Some(_) -> make_tree(Zipper(..zipper, cur: zipper.top))
    option.None -> Leaf
  }
}

fn make_tree(remains: Zipper(a)) {
  case remains.cur {
    option.Some(a) -> {
      let assert Ok(#(l_ch, r_ch, _)) = remains.map |> dict.get(a)
      Node(
        a,
        make_tree(Zipper(..remains, cur: l_ch)),
        make_tree(Zipper(..remains, cur: r_ch)),
      )
    }
    option.None -> Leaf
  }
}

pub fn value(zipper: Zipper(a)) -> Result(a, Nil) {
  case zipper.cur {
    option.Some(a) -> Ok(a)
    option.None -> Error(Nil)
  }
}

pub fn up(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  case zipper.cur {
    option.Some(a) -> {
      case zipper.map |> dict.get(a) {
        Ok(#(_, _, option.Some(v))) -> Ok(Zipper(..zipper, cur: option.Some(v)))
        _ -> Error(Nil)
      }
    }
    option.None -> Error(Nil)
  }
}

pub fn left(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  case zipper.cur {
    option.Some(a) -> {
      case zipper.map |> dict.get(a) {
        Ok(#(option.Some(v), _, _)) -> Ok(Zipper(..zipper, cur: option.Some(v)))
        _ -> Ok(Zipper(..zipper, cur: option.None))
      }
    }
    option.None -> Error(Nil)
  }
}

pub fn right(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  case zipper.cur {
    option.Some(a) -> {
      case zipper.map |> dict.get(a) {
        Ok(#(_, option.Some(v), _)) -> Ok(Zipper(..zipper, cur: option.Some(v)))
        _ -> Error(Nil)
      }
    }
    option.None -> Error(Nil)
  }
}

pub fn set_value(zipper: Zipper(a), value: a) -> Zipper(a) {
  let assert option.Some(a) = zipper.cur

  let new_map = case zipper.map |> dict.get(a) {
    Ok(#(l, r, p)) -> {
      let new_map =
        zipper.map |> dict.delete(a) |> dict.insert(value, #(l, r, p))

      case p {
        option.Some(p) -> {
          case new_map |> dict.get(p) {
            Ok(#(l, r, pp)) -> {
              case l == zipper.cur, r == zipper.cur {
                True, _ ->
                  new_map |> dict.insert(p, #(option.Some(value), r, pp))
                _, True ->
                  new_map |> dict.insert(p, #(l, option.Some(value), pp))
                _, _ -> new_map
              }
            }
            _ -> new_map
          }
        }
        _ -> new_map
      }
    }
    _ -> {
      zipper.map
      |> dict.insert(value, #(option.None, option.None, option.Some(a)))
    }
  }

  Zipper(..zipper, cur: option.Some(value), map: new_map)
}

pub fn set_left(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  case zipper.cur {
    option.Some(a) -> {
      let m_i = zipper.map |> dict.get(a)
      case m_i {
        Ok(#(_, pr, pp)) -> {
          case tree {
            Leaf -> {
              let new_map = zipper.map |> dict.insert(a, #(option.None, pr, pp))
              Ok(Zipper(..zipper, map: new_map))
            }
            Node(v, _, _) -> {
              let new_map = make_zipper_map(dict.new(), tree, zipper.cur)
              let new_map = zipper.map |> dict.merge(new_map)
              let new_map = new_map |> dict.insert(a, #(option.Some(v), pr, pp))

              Ok(Zipper(..zipper, map: new_map))
            }
          }
        }
        _ -> {
          case tree {
            Leaf -> {
              let new_map =
                zipper.map
                |> dict.insert(a, #(option.None, option.None, option.None))

              Ok(Zipper(..zipper, map: new_map))
            }
            Node(v, _, _) -> {
              let new_map = make_zipper_map(dict.new(), tree, zipper.cur)
              let new_map = zipper.map |> dict.merge(new_map)
              let new_map =
                new_map
                |> dict.insert(a, #(option.Some(v), option.None, option.None))

              Ok(Zipper(..zipper, map: new_map))
            }
          }
        }
      }
    }
    option.None -> Error(Nil)
  }
}

pub fn set_right(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  case zipper.cur {
    option.Some(a) -> {
      let m_i = zipper.map |> dict.get(a)
      case m_i {
        Ok(#(pl, _, pp)) -> {
          case tree {
            Leaf -> {
              let new_map = zipper.map |> dict.insert(a, #(pl, option.None, pp))
              Ok(Zipper(..zipper, map: new_map))
            }
            Node(v, _, _) -> {
              let new_map = make_zipper_map(dict.new(), tree, zipper.cur)
              let new_map = zipper.map |> dict.merge(new_map)
              let new_map = new_map |> dict.insert(a, #(pl, option.Some(v), pp))

              Ok(Zipper(..zipper, map: new_map))
            }
          }
        }
        _ -> {
          case tree {
            Leaf -> {
              let new_map =
                zipper.map
                |> dict.insert(a, #(option.None, option.None, option.None))
              Ok(Zipper(..zipper, map: new_map))
            }
            Node(v, _, _) -> {
              let new_map = make_zipper_map(dict.new(), tree, zipper.cur)
              let new_map = zipper.map |> dict.merge(new_map)
              let new_map =
                new_map
                |> dict.insert(a, #(option.None, option.Some(v), option.None))

              Ok(Zipper(..zipper, map: new_map))
            }
          }
        }
      }
    }
    option.None -> Error(Nil)
  }
}
// pub fn main() {
//   let tree =
//     Node(
//       value: 1,
//       left: Node(
//         value: 2,
//         left: Leaf,
//         right: Node(value: 3, left: Leaf, right: Leaf),
//       ),
//       right: Node(value: 4, left: Leaf, right: Leaf),
//     )

//   echo to_zipper(tree)
//     |> left
//     |> result.unwrap(Zipper(dict.new(), option.None, option.None))
//     |> left
//   // |> to_tree
// }
