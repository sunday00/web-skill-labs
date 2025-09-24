import gleam/int
import gleam/list

pub type Category {
  Ones
  Twos
  Threes
  Fours
  Fives
  Sixes
  FullHouse
  FourOfAKind
  LittleStraight
  BigStraight
  Choice
  Yacht
}

type SB {
  Big
  Small
}

pub fn score(category: Category, dice: List(Int)) -> Int {
  let dice = dice |> list.sort(int.compare)
  case category {
    FullHouse -> {
      case is_yacht(dice) {
        True -> 0
        False -> {
          case is_full_house(dice) {
            False -> 0
            True -> sum(dice)
          }
        }
      }
    }
    FourOfAKind -> {
      case is_four_cards(dice) {
        True -> four_cards_sum(dice)
        False -> 0
      }
    }
    LittleStraight -> {
      case is_straight(Small, dice) {
        True -> 30
        False -> 0
      }
    }
    BigStraight -> {
      case is_straight(Big, dice) {
        True -> 30
        False -> 0
      }
    }
    Choice -> sum(dice)
    Yacht -> {
      case is_yacht(dice) {
        True -> 50
        False -> 0
      }
    }
    _ -> {
      let n = case category {
        Ones -> 1
        Twos -> 2
        Threes -> 3
        Fours -> 4
        Fives -> 5
        Sixes -> 6
        _ -> 0
      }

      dice |> list.filter(fn(x) { x == n }) |> sum
    }
  }
}

fn is_yacht(dice: List(Int)) {
  case dice {
    [f, ..] if [f, f, f, f, f] == dice -> True
    _ -> False
  }
}

fn is_full_house(dice: List(Int)) {
  let assert Ok(f) = dice |> list.first

  case
    dice
    |> list.chunk(fn(n) { n == f })
  {
    [[_, _, _], [a, b]] if a == b -> True
    [[_, _], [a, b, c]] if a == b && b == c && c == a -> True
    _ -> False
  }
}

fn is_straight(sb: SB, dice: List(Int)) {
  case sb, dice {
    Big, [2, 3, 4, 5, 6] -> True
    Small, [1, 2, 3, 4, 5] -> True
    _, _ -> False
  }
}

fn is_four_cards(dice: List(Int)) {
  let assert Ok(f) = dice |> list.first

  case
    dice
    |> list.chunk(fn(n) { n == f })
  {
    [[_, _, _, _], [_]] -> True
    [[_], [_, _, _, _]] -> True
    [[_, _, _, _, _]] -> True
    _ -> False
  }
}

fn four_cards_sum(dice: List(Int)) {
  case is_yacht(dice) {
    True -> {
      sum(dice |> list.drop(1))
    }
    False -> {
      let assert Ok(f) = dice |> list.first

      let assert [a, b] =
        dice
        |> list.chunk(fn(n) { n == f })

      case a |> list.length, b |> list.length {
        la, _ if la == 4 -> sum(a)
        _, lb if lb == 4 -> sum(b)
        _, _ -> 0
      }
    }
  }
}

fn sum(dice: List(Int)) {
  dice |> list.fold(0, fn(acc, i) { acc + i })
}

pub fn main() {
  echo score(FourOfAKind, [6, 6, 4, 6, 6])
}
