package chess.util

import chess.player.Player

class PlayerSelector(var first: Player, var second: Player) {

  def next(): Player = {
    val t = first
    first = second
    second = t
    t
  }
}
