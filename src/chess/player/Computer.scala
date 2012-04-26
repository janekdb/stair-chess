package chess.player

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }

class Computer(val board: BoardModel, val colour: Colour) extends Player {

  implicit def stringToMovePiece(s: String) = new MovePiece(s)

  var moves: List[Move] = List(
    "a2a3",
    "b2b4",
    "c2c4",
    "h2h3",
    /* Knight */
    "b1c3",
    /* Knight */
    "g1f3",
    /* Knight takes pawn */
    "c3d5",
    "c1b2",
    "b4b5")

  def getMove: Move = {
    val m :: ms = moves
    moves = ms
    m
  }
}