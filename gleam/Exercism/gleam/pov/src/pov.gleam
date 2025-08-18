import gleam/dict.{type Dict}
import gleam/list

pub type Tree(a) {
  Tree(label: a, children: List(Tree(a)))
}

pub fn from_pov(tree: Tree(a), from: a) -> Result(Tree(a), Nil) {
  let map = make_map(#(dict.new(), dict.new()), tree)

  let new_root_tree_el = map.0 |> dict.get(from)

  case new_root_tree_el {
    Ok(_) -> {
      echo make_tree(Tree(from, []), map)

      todo
    }
    _ -> Error(Nil)
  }
}

pub fn path_to(
  tree tree: Tree(a),
  from from: a,
  to to: a,
) -> Result(List(a), Nil) {
  todo
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

fn make_tree(acc: Tree(a), map: #(Dict(a, List(a)), Dict(a, a))) {
  let acc = case map.0 |> dict.get(acc.label) {
    Ok(children) -> {
      Tree(
        acc.label,
        children
          |> list.fold([], fn(ac, cu) {
            ac
            |> list.prepend(make_tree(Tree(cu, []), map))
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

pub fn main() {
  from_pov(
    Tree(label: "grandparent", children: [
      Tree(label: "parent", children: [
        Tree(label: "x", children: [Tree("kid-0", []), Tree("kid-1", [])]),
        Tree("sibling-0", []),
        Tree("sibling-1", []),
      ]),
      Tree(label: "uncle", children: [
        Tree("cousin-0", []),
        Tree("cousin-1", []),
      ]),
    ]),
    "x",
  )
}
