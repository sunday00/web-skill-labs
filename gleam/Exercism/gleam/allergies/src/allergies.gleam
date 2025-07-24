import gleam/list

pub type Allergen {
  Eggs
  Peanuts
  Shellfish
  Strawberries
  Tomatoes
  Chocolate
  Pollen
  Cats
}

fn get_allergen_by_n(n: Int) {
  case n {
    n if n == 1 -> Ok(Eggs)
    n if n == 2 -> Ok(Peanuts)
    n if n == 4 -> Ok(Shellfish)
    n if n == 8 -> Ok(Strawberries)
    n if n == 16 -> Ok(Tomatoes)
    n if n == 32 -> Ok(Chocolate)
    n if n == 64 -> Ok(Pollen)
    n if n == 128 -> Ok(Cats)
    _ -> Error(Nil)
  }
}

pub fn allergic_to(allergen: Allergen, score: Int) -> Bool {
  todo
}

pub fn list(score: Int) -> List(Allergen) {
  reducer([], score)
}

fn reducer(acc: List(Allergen), remain: Int) {
  let max = get_max_powers(remain, 1)

  case max {
    m if m <= 0 || remain <= 0 -> acc
    m if m >= 256 -> {
      reducer(acc, remain - max)
    }
    _ -> {
      let assert Ok(al) = get_allergen_by_n(max)

      reducer(acc |> list.prepend(al), remain - max)
    }
  }
}

fn get_max_powers(max: Int, cur: Int) {
  case cur * 2 {
    c if c > max -> cur
    _ -> get_max_powers(max, cur * 2)
  }
}

pub fn main() {
  echo list(248)
}
