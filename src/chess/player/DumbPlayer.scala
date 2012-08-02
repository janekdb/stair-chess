package chess.player

import chess.model.Move
import chess.model.Configuration

class DumbPlayer(var moves: List[Move]) extends Player {

  def getName = "DumbPlayer"

  def getMove(configuration: Configuration): Option[Move] = {
    val m :: ms = moves
    moves = ms
    Some(m)
  }

}