import gleam/list
import gleam/result

pub type Comparison {
  Equal
  Unequal
  Sublist
  Superlist
}

pub fn sublist(compare list_a: List(a), to list_b: List(a)) -> Comparison {
  case list_a, list_b {
    a, b if a == b -> Equal
    [], [] -> Equal
    _, [] -> Superlist
    [], _ -> Sublist
    _, _ -> {
      let i_a = list_a |> list.index_map(fn(n, i) { #(n, i) })
      let i_b = list_b |> list.index_map(fn(n, i) { #(n, i) })

      let r_a = is_b_includes_all_a(list_a, i_b, [-1])
      let r_b = is_b_includes_all_a(list_b, i_a, [-1])

      case r_a, r_b {
        True, False -> Sublist
        False, True -> Superlist
        _, _ -> Unequal
      }
    }
  }
}

fn is_b_includes_all_a(a: List(a), b: List(#(a, Int)), i: List(Int)) {
  case i == [-1] {
    True -> {
      let assert Ok(f) = a |> list.first
      let rem = a |> list.drop(1)

      case b |> list.key_filter(f) {
        [] -> False
        br -> {
          is_b_includes_all_a(rem, b, br |> list.map(fn(el) { el + 1 }))
        }
      }
    }
    False -> {
      case a {
        [] -> True
        [f, ..r] -> {
          let nxt = b |> list.key_filter(f)
          let nxt = nxt |> list.filter(fn(n) { i |> list.contains(n) })
          case nxt |> list.length > 0 {
            True -> is_b_includes_all_a(r, b, i |> list.map(fn(el) { el + 1 }))
            False -> False
          }
        }
      }
    }
  }
}
