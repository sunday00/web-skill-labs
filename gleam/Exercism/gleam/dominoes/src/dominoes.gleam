import gleam/int
import gleam/list
import gleam/result

pub fn can_chain(chain: List(#(Int, Int))) -> Bool {
  case chain {
    [] -> True
    [f] -> f.0 == f.1
    [f, s] -> {
      let assert [f1, f2] = [f.0, f.1] |> list.sort(int.compare)
      let assert [s1, s2] = [s.0, s.1] |> list.sort(int.compare)

      f1 == s1 && f2 == s2
    }
    [f, ..r] -> {
      reducer(r |> list.index_map(fn(t, i) { #(i, t.0, t.1) }), [f])
    }
  }
}

fn reducer(chain: List(#(Int, Int, Int)), cur: List(#(Int, Int))) {
  case chain {
    [] -> {
      case cur {
        [] -> False
        _ -> {
          let assert Ok(first) = cur |> list.first
          let assert Ok(last) = cur |> list.last

          first.0 == last.0
          || first.0 == last.1
          || first.1 == last.0
          || first.1 == last.1
        }
      }
    }
    _ -> {
      let assert Ok(last) = list.last(cur)
      let possible_nxt =
        chain
        |> list.filter(fn(el) { el.1 == last.1 || el.2 == last.1 })

      let res_list =
        possible_nxt
        |> list.map(fn(el) {
          let sorted = case el {
            el if el.1 == last.1 -> #(el.1, el.2)
            _ -> #(el.2, el.1)
          }

          reducer(
            chain |> list.filter(fn(i) { i.0 != el.0 }),
            cur |> list.append([sorted]),
          )
        })

      res_list |> list.contains(True)
    }
  }
}
// pub fn main() {
//   echo can_chain([#(1, 2), #(2, 3), #(3, 1), #(4, 5), #(5, 6), #(6, 4)])
// }
