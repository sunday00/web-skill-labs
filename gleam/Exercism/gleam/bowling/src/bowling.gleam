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
  case knocked_pins > 10 || knocked_pins < 0 {
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
                    [10, 0], [f], True -> f + knocked_pins
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
                [10, 0] -> {
                  Ok(Game(
                    game.frames
                    |> list.append([
                      Frame([knocked_pins], [knocked_pins, strike_bonus]),
                    ]),
                  ))
                }
                [f] if f + knocked_pins > 10 -> Error(InvalidPinCount)
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
                _ -> {
                  case knocked_pins == 10 && game.frames |> list.length < 9 {
                    True -> {
                      let new_frames =
                        game.frames
                        |> list.append([Frame([10, 0], [strike_bonus])])
                      Ok(Game(new_frames))
                    }
                    False -> {
                      let new_frames =
                        game.frames
                        |> list.append([Frame([knocked_pins], [strike_bonus])])
                      Ok(Game(new_frames))
                    }
                  }
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
              case f == 10 {
                True -> {
                  let new_frame =
                    Frame(last.rolls |> list.append([knocked_pins]), last.bonus)

                  Ok(Game(
                    game.frames
                    |> list.reverse
                    |> list.drop(1)
                    |> list.prepend(new_frame)
                    |> list.reverse,
                  ))
                }
                False -> {
                  case f + knocked_pins > 10 {
                    True -> Error(InvalidPinCount)
                    False -> {
                      let new_frame = Frame([f, knocked_pins], last.bonus)

                      Ok(Game(
                        game.frames
                        |> list.reverse
                        |> list.drop(1)
                        |> list.prepend(new_frame)
                        |> list.reverse,
                      ))
                    }
                  }
                }
              }
            }
            [f, s] -> {
              case f, s {
                f, s if f == 10 && s == 10 -> {
                  let new_frame = Frame([10, 10, knocked_pins], last.bonus)

                  Ok(Game(
                    game.frames
                    |> list.reverse
                    |> list.drop(1)
                    |> list.prepend(new_frame)
                    |> list.reverse,
                  ))
                }
                f, s if f == 10 && s < 10 -> {
                  // echo #(f, s, knocked_pins)
                  case s + knocked_pins > 10 {
                    True -> Error(InvalidPinCount)
                    False -> {
                      let new_frame = Frame([10, s, knocked_pins], last.bonus)

                      Ok(Game(
                        game.frames
                        |> list.reverse
                        |> list.drop(1)
                        |> list.prepend(new_frame)
                        |> list.reverse,
                      ))
                    }
                  }
                }
                f, s if f < 10 && s == 10 -> Error(InvalidPinCount)
                f, s if f < 10 && s < 10 -> {
                  case f + s == 10 {
                    True -> {
                      let new_frame = Frame([f, s, knocked_pins], last.bonus)

                      Ok(Game(
                        game.frames
                        |> list.reverse
                        |> list.drop(1)
                        |> list.prepend(new_frame)
                        |> list.reverse,
                      ))
                    }
                    False -> Error(GameComplete)
                  }
                }
                _, _ -> Error(InvalidPinCount)
              }
            }
            [f, s, t] -> Error(GameComplete)
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
      let last = game.frames |> list.last |> result.unwrap(Frame([], []))
      case last.rolls {
        [f] if f == 10 -> Error(GameNotComplete)
        [f, s] if f + s == 10 -> Error(GameNotComplete)
        [f, s] if f + s == 20 -> Error(GameNotComplete)
        _ -> {
          game.frames
          |> list.fold(0, fn(acc, cur) {
            acc
            + {
              cur.rolls
              |> list.fold(0, fn(ac, cu) { ac + cu })
            }
            + {
              cur.bonus
              |> list.fold(0, fn(ac, cu) { ac + cu })
            }
          })
          |> Ok
        }
      }
    }
  }
}

pub fn main() {
  // let rolls = [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  // let rolls = [0, 0]
  // let rolls = Game([Frame([5], [])])
  let rolls = Game(list.repeat(Frame([10], []), 11))
  // echo roll(rolls, 6)
  // echo rolls |> rolling()

  let rolls = roll(rolls, 10)
  echo rolls
  echo score(rolls |> result.unwrap(Game([Frame([], [])])))
}

fn rolling(rolls: List(Int)) {
  rolls
  |> list.fold(Game([]), fn(game, pins) {
    // echo #(game)
    let assert Ok(new_game) = roll(game, pins)
    new_game
  })
  // |> score
}

fn roll_and_last_roll_be_error(rolls: List(Int), last_roll: Int, error: Error) {
  rolls
  |> list.fold(Game([]), fn(game, pins) {
    let assert Ok(new_game) = roll(game, pins)
    new_game
  })
  |> roll(last_roll)
  |> should.equal(Error(error))
}
