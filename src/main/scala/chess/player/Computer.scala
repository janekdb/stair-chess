package chess.player

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }
import chess.model.Configuration

class Computer extends Player {

  implicit def stringToMovePiece(s: String): MovePiece = new MovePiece(s)

  var moves: List[Move] = List(
    "e2e4",
    "d1h5",
    "f1c4",
    "h5f7")

  def getName = "Computer"

  def getMove(configuration: Configuration): Option[Move] = {
    val m :: ms = moves
    moves = ms
    Some(m)
  }
}