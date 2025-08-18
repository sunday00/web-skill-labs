import gleam/dict.{type Dict}
import gleam/list

pub type Tree(a) {
  Tree(label: a, children: List(Tree(a)))
}

pub fn from_pov(tree: Tree(a), from: a) -> Result(Tree(a), Nil) {
  echo make_map(dict.new(), tree)

  Ok(Tree(from, []))
}

pub fn path_to(
  tree tree: Tree(a),
  from from: a,
  to to: a,
) -> Result(List(a), Nil) {
  todo
}

fn make_map(acc: Dict(a, List(String)), tree: Tree(a)) {
  todo
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
