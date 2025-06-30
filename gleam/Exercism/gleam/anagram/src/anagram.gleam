import gleam/list
import gleam/string

pub fn find_anagrams(word: String, candidates: List(String)) -> List(String) {
  candidates |> list.filter(fn(x) { reducer(word, x) })
}

pub fn reducer(word_raw: String, candidate_raw: String) {
  let word =
    word_raw
    |> string.lowercase
    |> string.to_graphemes
    |> list.sort(string.compare)
  let candidate =
    candidate_raw
    |> string.lowercase
    |> string.to_graphemes
    |> list.sort(string.compare)

  { word_raw |> string.lowercase != candidate_raw |> string.lowercase }
  && { word == candidate }
}
