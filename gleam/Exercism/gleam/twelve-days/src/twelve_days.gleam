import gleam/list
import gleam/string

pub fn verse(number: Int) -> String {
  case number {
    n if n == 1 ->
      "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
    n if n == 2 ->
      "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree."
    n if n == 3 ->
      "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."

    n if n == 4 ->
      "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
    n if n == 5 ->
      "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
    n if n == 6 ->
      "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."

    n if n == 7 ->
      "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
    n if n == 8 ->
      "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
    n if n == 9 ->
      "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."

    n if n == 10 ->
      "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
    n if n == 11 ->
      "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
    n if n == 12 ->
      "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."

    _ -> ""
  }
}

pub fn lyrics(from starting_verse: Int, to ending_verse: Int) -> String {
  reducer([], starting_verse, ending_verse)
  |> list.map(fn(i) { verse(i) })
  |> string.join("\n")
}

fn reducer(acc: List(Int), cur: Int, max: Int) {
  case cur {
    c if c == max -> acc |> list.append([cur])
    c if c < max -> reducer(acc |> list.append([cur]), cur + 1, max)
    _ -> []
  }
}
