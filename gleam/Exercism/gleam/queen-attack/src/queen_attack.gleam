import gleam/int

pub type Position {
  Position(row: Int, column: Int)
}

pub type Error {
  RowTooSmall
  RowTooLarge
  ColumnTooSmall
  ColumnTooLarge
}

pub fn create(queen: Position) -> Result(Nil, Error) {
  case queen.column, queen.row {
    c, _r if c < 0 -> Error(ColumnTooSmall)
    _c, r if r < 0 -> Error(RowTooSmall)
    c, _r if c > 7 -> Error(ColumnTooLarge)
    _c, r if r > 7 -> Error(RowTooLarge)
    _, _ -> Ok(Nil)
  }
}

pub fn can_attack(
  black_queen black_queen: Position,
  white_queen white_queen: Position,
) -> Bool {
  case
    white_queen.column,
    white_queen.row,
    black_queen.column,
    black_queen.row
  {
    wc, wr, bc, br if wc == bc || wr == br -> True
    wc, wr, bc, br -> {
      let r_diff = wr - br |> int.absolute_value
      let c_diff = wc - bc |> int.absolute_value

      r_diff == c_diff
    }
  }
}
