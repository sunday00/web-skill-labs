import gleam/list
import gleam/string

pub type Robot {
  Robot(direction: Direction, position: Position)
}

pub type Direction {
  North
  East
  South
  West
}

pub type Position {
  Position(x: Int, y: Int)
}

pub fn create(direction: Direction, position: Position) -> Robot {
  Robot(direction, position)
}

pub fn move(
  direction: Direction,
  position: Position,
  instructions: String,
) -> Robot {
  instructions
  |> string.to_graphemes
  |> list.fold(create(direction, position), obey)
}

fn obey(r: Robot, cur: String) {
  case cur {
    "R" -> {
      case r.direction {
        North -> {
          create(East, r.position)
        }
        East -> {
          create(South, r.position)
        }
        South -> {
          create(West, r.position)
        }
        West -> {
          create(North, r.position)
        }
      }
    }
    "L" -> {
      case r.direction {
        North -> {
          create(West, r.position)
        }
        East -> {
          create(North, r.position)
        }
        South -> {
          create(East, r.position)
        }
        West -> {
          create(South, r.position)
        }
      }
    }
    "A" -> {
      case r.direction {
        North -> {
          create(r.direction, Position(r.position.x, r.position.y + 1))
        }
        East -> {
          create(r.direction, Position(r.position.x + 1, r.position.y))
        }
        South -> {
          create(r.direction, Position(r.position.x, r.position.y - 1))
        }
        West -> {
          create(r.direction, Position(r.position.x - 1, r.position.y))
        }
      }
    }
    _ -> r
  }
}
