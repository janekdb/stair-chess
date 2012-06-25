package chess.player

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }
import chess.model.Promote
import chess.model.Configuration

class Human(val board: BoardModel, val colour: Colour) extends Player {

  implicit def stringToMovePiece(s: String) = new MovePiece(s)

  var moves: List[Move] = List(
      "e7e5",
      "b8c6",
      "g8f6"
    )
    
  def getMove(configuration: Configuration): Option[Move] = {
    val m :: ms = moves
    moves = ms
    Some(m)
  }
}