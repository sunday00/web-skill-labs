import gleam/int
import gleam/result
import gleam/string

pub type Clock {
  Clock(hour: Int, minute: Int)
}

fn clock_calculator(hour: Int, minute: Int, add_minute: Int) {
  let tm = minute + add_minute
  let m = { 60 + { tm % 60 } } % 60
  let th =
    tm
    |> int.floor_divide(60)
    |> result.unwrap(0)
    |> int.add(hour)
    |> int.remainder(24)
    |> result.unwrap(0)
  let h = { 24 + { th % 24 } } % 24

  #(h, m)
}

pub fn create(hour hour: Int, minute minute: Int) -> Clock {
  let #(h, m) = clock_calculator(hour, minute, 0)

  Clock(h, m)
}

pub fn add(clock: Clock, minutes minutes: Int) -> Clock {
  let #(h, m) = clock_calculator(clock.hour, clock.minute, minutes)

  Clock(h, m)
}

pub fn subtract(clock: Clock, minutes minutes: Int) -> Clock {
  let #(h, m) = clock_calculator(clock.hour, clock.minute, minutes * -1)

  Clock(h, m)
}

pub fn display(clock: Clock) -> String {
  let #(h, m) = clock_calculator(clock.hour, clock.minute, 0)

  h |> int.to_string |> string.pad_left(2, "0")
  <> ":"
  <> m |> int.to_string |> string.pad_left(2, "0")
}
