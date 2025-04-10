import gleam/float
import gleam/list
import gleam/order.{type Order}

pub type City {
  City(name: String, temperature: Temperature)
}

pub type Temperature {
  Celsius(Float)
  Fahrenheit(Float)
}

pub fn fahrenheit_to_celsius(f: Float) -> Float {
  { f -. 32.0 } /. 1.8
}

fn sure_celisius(t: Temperature) -> Float {
  case t {
    Celsius(f) -> f
    Fahrenheit(f) -> fahrenheit_to_celsius(f)
  }
}

pub fn compare_temperature(left: Temperature, right: Temperature) -> Order {
  let l = sure_celisius(left)
  let r = sure_celisius(right)

  l |> float.compare(r)
}

pub fn sort_cities_by_temperature(cities: List(City)) -> List(City) {
  cities
  |> list.sort(by: fn(c1: City, c2: City) {
    compare_temperature(c1.temperature, c2.temperature)
  })
}
