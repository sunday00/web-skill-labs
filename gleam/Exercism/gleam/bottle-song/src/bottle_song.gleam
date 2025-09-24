import gleam/list
import gleam/string

pub fn recite(
  start_bottles start_bottles: Int,
  take_down take_down: Int,
) -> String {
  let m =
    [
      "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
      "Ten",
    ]
    |> list.index_map(fn(el, i) { #(i + 1, el) })

  list.repeat(
    "%START% green bottles hanging on the wall,
%START% green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be %REM% green bottles hanging on the wall.",
    take_down,
  )
  |> list.index_map(fn(el, i) {
    let assert Ok(start) = m |> list.key_find(start_bottles - i)

    let rem = case m |> list.key_find({ start_bottles - i } - 1) {
      Ok(rr) -> rr |> string.lowercase
      _ -> "no"
    }

    el
    |> string.replace("%START%", start)
    |> string.replace("%REM%", rem)
    |> string.replace("one green bottles", "one green bottle")
    |> string.replace("One green bottles", "One green bottle")
  })
  |> string.join("\n\n")
}
