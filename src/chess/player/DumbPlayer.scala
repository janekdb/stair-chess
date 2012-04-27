package chess.player

import chess.model.Move

class DumbPlayer(var moves: List[Move]) extends Player {

  def getMove: Move = {
    val m :: ms = moves
    moves = ms
    m
  }

}