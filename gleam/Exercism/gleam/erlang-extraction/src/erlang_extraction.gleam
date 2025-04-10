// Please define the GbTree type
pub type GbTree(k, v)

@external(erlang, "gb_trees", "empty")
pub fn new_gb_tree() -> GbTree(k, v)

@external(erlang, "gb_trees", "insert")
fn insert_via_erlang(key: k, value: v, tree: GbTree(k, v)) -> GbTree(k, v)

pub fn insert(tree: GbTree(k, v), key: k, value: v) -> GbTree(k, v) {
  insert_via_erlang(key, value, tree)
}

@external(erlang, "gb_trees", "delete_any")
fn delete_via_erlang(key: k, tree: GbTree(k, v)) -> GbTree(k, v)

pub fn delete(tree: GbTree(k, v), key: k) -> GbTree(k, v) {
  delete_via_erlang(key, tree)
}
