import gleam/result

pub type Player {
  Black
  White
}

pub type Game {
  Game(
    white_captured_stones: Int,
    black_captured_stones: Int,
    player: Player,
    error: String,
  )
}

fn switc_player(game: Game) {
  let p = case game.player {
    White -> Black
    Black -> White
  }

  Game(..game, player: p)
}

pub fn apply_rules(
  game: Game,
  rule1: fn(Game) -> Result(Game, String),
  rule2: fn(Game) -> Game,
  rule3: fn(Game) -> Result(Game, String),
  rule4: fn(Game) -> Result(Game, String),
) -> Game {
  // case game |> rule1 {
  //   Ok(ng1) -> {
  //     let ng2 = ng1 |> rule2

  //     case ng2 |> rule3 {
  //       Ok(ng3) -> {
  //         case ng3 |> rule4 {
  //           Ok(ng4) -> {
  //             let p = case ng4.player {
  //               White -> Black
  //               Black -> White
  //             }

  //             Game(..ng4, player: p)
  //           }
  //           Error(s) -> Game(..game, error: s)
  //         }
  //       }
  //       Error(s) -> Game(..game, error: s)
  //     }
  //   }
  //   Error(s) -> Game(..game, error: s)
  // }

  let r =
    game |> rule1 |> result.map(rule2) |> result.try(rule3) |> result.try(rule4)

  case r {
    Ok(g) -> switc_player(g)
    Error(e) -> Game(..game, error: e)
  }
}
