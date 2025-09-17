import exercism/should
import gleam/int
import gleam/list
import gleam/result

pub opaque type Frame {
  Frame(rolls: List(Int), bonus: List(Int))
}

pub type Game {
  Game(frames: List(Frame))
}

pub type Error {
  InvalidPinCount
  GameComplete
  GameNotComplete
}

pub fn roll(game: Game, knocked_pins: Int) -> Result(Game, Error) {
  // echo #(game, knocked_pins)
  case knocked_pins > 10 {
    True -> Error(InvalidPinCount)
    False -> {
      case game.frames |> list.length < 10 {
        True -> {
          case game.frames |> list.last {
            Ok(last) -> {
              let old_last =
                game.frames |> list.reverse |> list.drop(1) |> list.first
              let strike_bonus = case old_last {
                Ok(ol) -> {
                  case ol.rolls, last.rolls, game.frames |> list.length < 10 {
                    [10], [f], True -> f + knocked_pins
                    _, _, _ -> 0
                  }
                }
                Error(_) -> 0
              }

              case last.rolls {
                [f, s] if f + s == 10 -> {
                  Ok(Game(
                    game.frames
                    |> list.append([
                      Frame([knocked_pins], [strike_bonus, knocked_pins]),
                    ]),
                  ))
                }
                [10] -> {
                  Ok(Game(
                    game.frames
                    |> list.append([Frame([knocked_pins], [strike_bonus])]),
                  ))
                }
                [f] if f < 10 -> {
                  let new_frames =
                    game.frames
                    |> list.reverse
                    |> list.drop(1)
                    |> list.prepend(Frame(
                      [f, knocked_pins],
                      last.bonus |> list.prepend(strike_bonus),
                    ))
                    |> list.reverse
                  Ok(Game(new_frames))
                }
                [f] if f + knocked_pins > 10 -> Error(InvalidPinCount)
                _ -> {
                  let new_frames =
                    game.frames
                    |> list.append([Frame([knocked_pins], [strike_bonus])])
                  Ok(Game(new_frames))
                }
              }
            }
            Error(_) -> {
              let nf = Frame([knocked_pins], [])
              Ok(Game([nf]))
            }
          }
        }
        False -> {
          let last = game.frames |> list.last |> result.unwrap(Frame([], []))
          case last.rolls {
            [f] -> {
              todo
            }
            [f, s] -> {
              todo
            }
            _ -> {
              todo
            }
          }
        }
      }
    }
  }
}

pub fn score(game: Game) -> Result(Int, Error) {
  case game.frames |> list.length {
    l if l < 10 -> Error(GameNotComplete)
    _ -> {
      echo game.frames

      // let res = game.frames |> list.fold(0, fn(acc, el){

      // })

      todo
    }
  }
}

pub fn main() {
  // let rolls = [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  // let rolls = [0, 0]
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 5]

  let rolled = rolling(rolls)

  echo roll(rolled, 6)

  echo score(rolled)
}

fn rolling(rolls: List(Int)) {
  rolls
  |> list.fold(Game([]), fn(game, pins) {
    let assert Ok(new_game) = roll(game, pins)
    new_game
  })
}
