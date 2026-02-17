package chess.player

import chess.model.{Move, MovePiece}
import chess.model.Configuration

class Computer extends Player {

  given Conversion[String, MovePiece] = new MovePiece(_)

  var moves: List[Move] = List("e2e4", "d1h5", "f1c4", "h5f7")

  def getName = "Computer"

  def getMove(configuration: Configuration): Option[Move] = {
    val move = moves.headOption
    moves = moves.tail
    move
  }
}
