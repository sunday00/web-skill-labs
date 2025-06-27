// import gleam/float
// import gleam/result

pub type Planet {
  Mercury
  Venus
  Earth
  Mars
  Jupiter
  Saturn
  Uranus
  Neptune
}

pub fn age(planet: Planet, seconds: Float) -> Float {
  // let earch_seconds = 31_557_600.0

  seconds
  /. 31_557_600.0
  /. case planet {
    // Mercury -> calc(seconds, float.multiply(earch_seconds, 0.2408467))
    // Venus -> calc(seconds, float.multiply(earch_seconds, 0.61519726))
    // Earth -> calc(seconds, earch_seconds)
    // Mars -> calc(seconds, float.multiply(earch_seconds, 1.8808158))
    // Jupiter -> calc(seconds, float.multiply(earch_seconds, 11.862615))
    // Saturn -> calc(seconds, float.multiply(earch_seconds, 29.447498))
    // Uranus -> calc(seconds, float.multiply(earch_seconds, 84.016846))
    // Neptune -> calc(seconds, float.multiply(earch_seconds, 164.79132))
    // Mercury -> calc(seconds, earch_seconds *. 0.2408467)
    // Venus -> calc(seconds, earch_seconds *. 0.61519726)
    // Earth -> calc(seconds, earch_seconds)
    // Mars -> calc(seconds, earch_seconds *. 1.8808158)
    // Jupiter -> calc(seconds, earch_seconds *. 11.862615)
    // Saturn -> calc(seconds, earch_seconds *. 29.447498)
    // Uranus -> calc(seconds, earch_seconds *. 84.016846)
    // Neptune -> calc(seconds, earch_seconds *. 164.79132)
    // Mercury -> calc(seconds, 0.2408467)
    // Venus -> calc(seconds, 0.61519726)
    // Earth -> calc(seconds, 1.0)
    // Mars -> calc(seconds, 1.8808158)
    // Jupiter -> calc(seconds, 11.862615)
    // Saturn -> calc(seconds, 29.447498)
    // Uranus -> calc(seconds, 84.016846)
    // Neptune -> calc(seconds, 164.79132)
    Mercury -> 0.2408467
    Venus -> 0.61519726
    Earth -> 1.0
    Mars -> 1.8808158
    Jupiter -> 11.862615
    Saturn -> 29.447498
    Uranus -> 84.016846
    Neptune -> 164.79132
  }
}
// pub fn calc(seconds: Float, div: Float) {
//   seconds /. { 31_557_600.0 *. div }
//   // |> float.divide(div)
//   // |> result.unwrap(0.0)
//   // |> float.to_precision(2)
// }
