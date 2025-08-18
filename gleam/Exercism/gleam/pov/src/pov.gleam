import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/result

pub type Tree(a) {
  Tree(label: a, children: List(Tree(a)))
}

pub fn from_pov(tree: Tree(a), from: a) -> Result(Tree(a), Nil) {
  let map = make_map(#(dict.new(), dict.new()), tree)

  let new_root_tree_el = map.0 |> dict.get(from)

  case new_root_tree_el {
    Ok(_) -> {
      let map_1 = filter_map1(dict.new(), map.1, from)
      let map_0 = filter_map0(map.0, map_1, from)

      let res = make_tree(Tree(from, []), #(map_0, map_1))

      Ok(res)
    }
    _ -> Error(Nil)
  }
}

pub fn path_to(
  tree tree: Tree(a),
  from from: a,
  to to: a,
) -> Result(List(a), Nil) {
  let map = make_map(#(dict.new(), dict.new()), tree)

  case
    map.0 |> dict.get(from),
    map.1 |> dict.get(from),
    map.0 |> dict.get(to),
    map.1 |> dict.get(to)
  {
    Error(_), Error(_), _, _ -> Error(Nil)
    _, _, Error(_), Error(_) -> Error(Nil)
    _, _, _, _ -> {
      let res = search([from], map, from, to)

      Ok(res)
    }
  }
}

fn make_map(acc: #(Dict(a, List(a)), Dict(a, a)), tree: Tree(a)) {
  let children = tree.children |> list.map(fn(el) { el.label })
  let acc_0 = acc.0 |> dict.insert(tree.label, children)
  let acc_1 =
    tree.children
    |> list.fold(acc.1, fn(ac, el) { ac |> dict.insert(el.label, tree.label) })

  case tree.children |> list.length > 0 {
    True -> {
      tree.children
      |> list.fold(#(acc_0, acc_1), fn(acc, el) { make_map(acc, el) })
    }
    False -> {
      #(acc.0 |> dict.insert(tree.label, []), acc.1)
    }
  }
}

fn filter_map1(acc: Dict(a, a), map: Dict(a, a), tree: a) {
  case map |> dict.get(tree) {
    Ok(p) -> {
      filter_map1(acc |> dict.insert(tree, p), map, p)
    }
    _ -> acc
  }
}

fn filter_map0(acc: Dict(a, List(a)), map1: Dict(a, a), tree: a) {
  case map1 |> dict.size > 0 {
    True -> {
      let assert Ok(p) = map1 |> dict.get(tree)
      let new_acc =
        acc
        |> dict.upsert(p, fn(li) {
          case li {
            option.Some(el) -> el |> list.filter(fn(e) { e != tree })
            option.None -> []
          }
        })
      let new_map1 = map1 |> dict.delete(tree)
      filter_map0(new_acc, new_map1, p)
    }
    _ -> acc
  }
}

fn make_tree(acc: Tree(a), map: #(Dict(a, List(a)), Dict(a, a))) {
  let acc = case map.0 |> dict.get(acc.label) {
    Ok(children) -> {
      Tree(
        acc.label,
        children
          |> list.fold([], fn(ac, cu) {
            ac
            |> list.prepend(
              make_tree(Tree(cu, []), #(map.0, map.1 |> dict.delete(cu))),
            )
            |> list.reverse
          }),
      )
    }
    _ -> {
      Tree(acc.label, [])
    }
  }

  case map.1 |> dict.get(acc.label) {
    Ok(parent_label) -> {
      let children =
        acc.children
        |> list.append([make_tree(Tree(parent_label, []), map)])
      Tree(acc.label, children)
    }
    _ -> acc
  }
}

fn search(acc: List(a), map: #(Dict(a, List(a)), Dict(a, a)), tree: a, fin: a) {
  case tree == fin {
    True -> acc
    False -> {
      let p = map.1 |> dict.get(tree)

      let pass_parent_acc = case p {
        Ok(pa) -> {
          // parent exists. filter alreay passed
          case acc |> list.contains(pa) {
            True -> {
              acc
            }
            False -> {
              // go upper parent
              search(acc |> list.append([pa]), map, pa, fin)
            }
          }
        }
        _ -> {
          // no parent. search child direct
          acc
        }
      }

      case pass_parent_acc |> list.contains(fin) {
        True -> pass_parent_acc
        False -> {
          let children = map.0 |> dict.get(tree) |> result.unwrap([])

          case children |> list.filter(fn(ch) { !{ list.contains(acc, ch) } }) {
            [] -> {
              pass_parent_acc
            }
            children -> {
              children
              |> list.map(fn(el) {
                search(acc |> list.append([el]), map, el, fin)
              })
              |> list.filter(fn(el) { el |> list.contains(fin) })
              |> list.sort(fn(a, b) {
                int.compare(list.length(a), list.length(b))
              })
              |> list.first
              |> result.unwrap([])
            }
          }
        }
      }
    }
  }
}

pub fn main() {
  path_to(
    tree: Tree(label: "parent", children: [
      Tree("a", []),
      Tree("x", []),
      Tree("b", []),
      Tree("c", []),
    ]),
    from: "a",
    to: "c",
  )
}
