import gleam/int
import gleam/option.{type Option, None, Some}

pub type Player {
  Player(name: Option(String), level: Int, health: Int, mana: Option(Int))
}

pub fn introduce(player: Player) -> String {
  case player.name {
    Some(s) -> s
    None -> "Mighty Magician"
  }
}

pub fn revive(player: Player) -> Option(Player) {
  case player.health {
    0 -> {
      case player.level {
        l if l < 10 -> Some(Player(..player, health: 100))
        _ -> Some(Player(..player, health: 100, mana: Some(100)))
      }
    }
    _ -> None
  }
}

pub fn cast_spell(player: Player, cost: Int) -> #(Player, Int) {
  case player.mana {
    Some(m) -> {
      case m {
        m if m < cost -> #(player, 0)
        _ -> #(Player(..player, mana: Some(m - cost)), cost * 2)
      }
    }
    None -> {
      // case player.health {
      //   h if h >= cost -> #(Player(..player, health: player.health - cost), 0)
      //   _ -> #(Player(..player, health: 0), 0)
      // }

      #(Player(..player, health: player.health - cost |> int.max(0)), 0)
    }
  }
}
