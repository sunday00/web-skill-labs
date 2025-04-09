import gleam/list
import gleam/result

// TODO: please define the Pizza custom type
pub type Pizza {
  Margherita
  Caprese
  Formaggio
  ExtraSauce(Pizza)
  ExtraToppings(Pizza)
}

fn one_pizza_nested(acc: Int, pizza: Pizza) {
  case pizza {
    ExtraSauce(p) -> {
      one_pizza_nested(acc + 1, p)
    }
    ExtraToppings(p) -> {
      one_pizza_nested(acc + 2, p)
    }
    Margherita -> acc + 7
    Caprese -> acc + 9
    Formaggio -> acc + 10
  }
}

pub fn pizza_price(pizza: Pizza) -> Int {
  one_pizza_nested(0, pizza)
}

pub fn order_price(order: List(Pizza)) -> Int {
  let prices =
    order
    |> list.map(pizza_price)
    |> list.reduce(fn(acc, cur) { acc + cur })
    |> result.unwrap(0)

  case order |> list.length {
    0 -> 0
    1 -> prices + 3
    2 -> prices + 2
    _ -> prices
  }
}
