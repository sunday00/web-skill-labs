import gleam/list

pub type Triplet {
  Triplet(Int, Int, Int)
}

pub fn triangle_parts(n: Int) -> List(List(Int)) {
  list.range(1, n)
  |> list.flat_map(fn(a) {
    list.range(a, n - a)
    |> list.flat_map(fn(b) {
      let c = n - a - b

      case c >= b && a + b > c && a != n {
        True -> [[a, b, c]]
        False -> []
      }
    })
  })
}

pub fn triplets_with_sum(sum: Int) -> List(Triplet) {
  sum
  |> triangle_parts
  |> list.filter(fn(l) {
    let assert [a, b, c] = l

    a * a + b * b == c * c
  })
  |> list.map(fn(l) {
    let assert [a, b, c] = l
    Triplet(a, b, c)
  })
}
