import gleam/list

pub fn place_location_to_treasure_location(
  place_location: #(String, Int),
) -> #(Int, String) {
  #(place_location.1, place_location.0)
}

pub fn treasure_location_matches_place_location(
  place_location: #(String, Int),
  treasure_location: #(Int, String),
) -> Bool {
  place_location.0 == treasure_location.1
  && place_location.1 == treasure_location.0
}

pub fn count_place_treasures(
  place: #(String, #(String, Int)),
  treasures: List(#(String, #(Int, String))),
) -> Int {
  let point = #(place.1.1, place.1.0)
  treasures |> list.count(fn(l) { l.1 == point })
}

pub fn special_case_swap_possible(
  found_treasure: #(String, #(Int, String)),
  place: #(String, #(String, Int)),
  desired_treasure: #(String, #(Int, String)),
) -> Bool {
  case found_treasure.0 {
    "Brass Spyglass" -> place.1 == #("B", 4)
    "Amethyst Octopus" ->
      place.1 == #("B", 5)
      && {
        desired_treasure.0 == "Crystal Crab"
        || desired_treasure.0 == "Glass Starfish"
      }
    "Vintage Pirate Hat" ->
      place.1 == #("A", 8)
      && {
        desired_treasure.0 == "Model Ship in Large Bottle"
        || desired_treasure.0 == "Antique Glass Fishnet Float"
      }
    _ -> False
  }
}
