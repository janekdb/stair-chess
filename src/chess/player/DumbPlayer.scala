package chess.player

import chess.model.Move

class DumbPlayer(var moves: List[Move]) extends Player {

  def getMove: Option[Move] = {
    val m :: ms = moves
    moves = ms
    Some(m)
  }

}