package chess.player

import chess.model.{Move, MovePiece}
import chess.model.Configuration

class Human extends Player {

  given Conversion[String, MovePiece] = new MovePiece(_)

  var moves: List[Move] = List("e7e5", "b8c6", "g8f6")

  def getName = "Human"

  def getMove(configuration: Configuration): Option[Move] = {
    val move = moves.headOption
    moves = moves.tail
    move
  }
}
