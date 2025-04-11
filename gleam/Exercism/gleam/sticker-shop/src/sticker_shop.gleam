import gleam/list

// Please define the Usd type

// Please define the Eur type

// Please define the Jpy type

pub type Usd

pub type Eur

pub type Jpy

// Please define the Money type

pub opaque type Money(currency) {
  Money(amount: Int)
}

pub fn dollar(amount: Int) -> Money(Usd) {
  Money(amount)
}

pub fn euro(amount: Int) -> Money(Eur) {
  Money(amount)
}

pub fn yen(amount: Int) -> Money(Jpy) {
  Money(amount)
}

pub fn total(prices: List(Money(currency))) -> Money(currency) {
  prices
  |> list.fold(0, fn(acc, cur) {
    let Money(amount) = cur
    acc + amount
  })
  |> Money(amount: _)
}
