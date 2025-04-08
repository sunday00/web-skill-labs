pub fn is_leap_year(year: Int) -> Bool {
  // case year % 4 == 0 {
  //   False -> False
  //   True -> {
  //     case year % 100 == 0 {
  //       False -> True
  //       True -> {
  //         case year % 400 == 0 {
  //           True -> True
  //           False -> False
  //         }
  //       }
  //     }
  //   }
  // }

  // year % 4 == 0 && year % 100 != 0 || year % 400 == 0

  case year % 400, year % 100, year % 4 {
    0, _, _ -> True
    _, 0, _ -> False
    _, _, 0 -> True
    _, _, _ -> False
  }
}
