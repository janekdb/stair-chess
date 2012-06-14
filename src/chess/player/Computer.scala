package chess.player

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }

class Computer(val board: BoardModel, val colour: Colour) extends Player {

  implicit def stringToMovePiece(s: String) = new MovePiece(s)

  var moves: List[Move] = List(
      "e2e4",
      "d1h5",
      "f1c4",
      "h5f7"
    )

  def getMove: Option[Move] = {
    val m :: ms = moves
    moves = ms
    Some(m)
  }
}