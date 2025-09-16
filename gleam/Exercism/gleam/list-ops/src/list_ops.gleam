pub fn append(first first: List(a), second second: List(a)) -> List(a) {
  append_repeater(second, first |> reverse)
}

fn append_repeater(acc: List(a), rem: List(a)) {
  case rem {
    [f, ..r] -> append_repeater([f, ..acc], r)
    [] -> acc
  }
}

pub fn concat(lists: List(List(a))) -> List(a) {
  concat_repeater([], lists)
}

fn concat_repeater(acc: List(a), rem: List(List(a))) {
  case rem {
    [f, ..r] -> concat_repeater(append(acc, f), r)
    [] -> acc
  }
}

pub fn filter(list: List(a), function: fn(a) -> Bool) -> List(a) {
  filter_repeater([], list, function)
}

fn filter_repeater(acc: List(a), rem: List(a), function: fn(a) -> Bool) {
  case rem {
    [f, ..r] -> {
      case function(f) {
        True -> filter_repeater(append(acc, [f]), r, function)
        False -> filter_repeater(acc, r, function)
      }
    }
    [] -> acc
  }
}

pub fn length(list: List(a)) -> Int {
  length_repeater(0, list)
}

fn length_repeater(acc: Int, rem: List(a)) {
  case rem {
    [_f, ..r] -> length_repeater(acc + 1, r)
    [] -> acc
  }
}

pub fn map(list: List(a), function: fn(a) -> b) -> List(b) {
  map_repeater([], list, function)
}

fn map_repeater(acc: List(b), rem: List(a), function: fn(a) -> b) {
  case rem {
    [f, ..r] -> {
      map_repeater(append(acc, [function(f)]), r, function)
    }
    [] -> acc
  }
}

pub fn foldl(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  fold_l_repeater(initial, list, function)
}

fn fold_l_repeater(acc: b, rem: List(a), function: fn(b, a) -> b) {
  case rem {
    [f, ..r] -> fold_l_repeater(function(acc, f), r, function)
    [] -> acc
  }
}

pub fn foldr(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  fold_l_repeater(initial, list |> reverse, function)
}

pub fn reverse(list: List(a)) -> List(a) {
  reverse_repeater([], list)
}

fn reverse_repeater(acc: List(a), rem: List(a)) {
  case rem {
    [f, ..r] -> {
      reverse_repeater([f, ..acc], r)
    }
    [] -> acc
  }
}
// pub fn main() {
//   echo map([1, 3, 5, 7], fn(x) { x + 1 })
// }
