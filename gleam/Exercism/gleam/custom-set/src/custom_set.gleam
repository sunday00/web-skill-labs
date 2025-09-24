import gleam/list

pub opaque type Set(t) {
  Set(members: List(t))
}

pub fn new(members: List(t)) -> Set(t) {
  Set(members |> list.unique)
}

pub fn is_empty(set: Set(t)) -> Bool {
  set.members |> list.length == 0
}

pub fn contains(in set: Set(t), this member: t) -> Bool {
  set.members |> list.contains(member)
}

pub fn is_subset(first: Set(t), of second: Set(t)) -> Bool {
  first.members
  |> list.map(fn(el) { second.members |> list.contains(el) })
  |> list.filter(fn(el) { el == False })
  |> list.length
  == 0
}

pub fn disjoint(first: Set(t), second: Set(t)) -> Bool {
  case is_empty(first), is_empty(second) {
    False, False -> {
      let it = intersection(first, second)
      it.members |> list.length == 0
    }
    _, _ -> True
  }
}

pub fn is_equal(first: Set(t), to second: Set(t)) -> Bool {
  is_subset(first, second) && is_subset(second, first)
}

pub fn add(to set: Set(t), this member: t) -> Set(t) {
  set.members |> list.prepend(member) |> Set
}

pub fn intersection(of first: Set(t), and second: Set(t)) -> Set(t) {
  first.members
  |> list.filter(fn(el) { second.members |> list.contains(el) })
  |> Set
}

pub fn difference(between first: Set(t), and second: Set(t)) -> Set(t) {
  first.members
  |> list.filter(fn(el) { !{ second.members |> list.contains(el) } })
  |> Set
}

pub fn union(of first: Set(t), and second: Set(t)) -> Set(t) {
  first.members |> list.append(second.members) |> list.unique |> Set
}
