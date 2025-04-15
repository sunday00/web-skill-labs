import gleam/int
import gleam/list

pub type Command {
  Wink
  DoubleBlink
  CloseYourEyes
  Jump
}

// fn reducer(acc: List(Command), idx: Command, bins: String) {
//   let nxt = case idx {
//     Wink -> DoubleBlink
//     DoubleBlink -> CloseYourEyes
//     CloseYourEyes -> Jump
//     Jump -> Jump
//   }

//   let b = bins |> string.drop_right(1)

//   case bins |> string.last {
//     Ok(n) if n == "1" -> {
//       case idx {
//         Jump -> [Jump, ..acc]
//         _ -> reducer([idx, ..acc], nxt, b)
//       }
//     }
//     _ -> {
//       case idx {
//         Jump -> acc
//         _ -> reducer(acc, nxt, b)
//       }
//     }
//   }
// }

pub fn commands(encoded_message: Int) -> List(Command) {
  [
    #(0b00000001, list.prepend(_, Wink)),
    #(0b00000010, list.prepend(_, DoubleBlink)),
    #(0b00000100, list.prepend(_, CloseYourEyes)),
    #(0b00001000, list.prepend(_, Jump)),
    #(0b00010000, list.reverse),
  ]
  |> list.fold([], fn(acc, cur) {
    let #(code, func) = cur

    case int.bitwise_and(code, encoded_message) {
      0 -> acc
      _ -> func(acc)
    }
  })
  |> list.reverse
}
