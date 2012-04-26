package chess.player

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }
import chess.model.Promote

class Human(val board: BoardModel, val colour: Colour) extends Player {

  implicit def stringToMovePiece(s: String) = new MovePiece(s)

  var moves: List[Move] = List(
    "a7a6",
    "d7d5",
    "e7e5",
    "f7f6",
    /* Queen */
    "d8d7",
    /* Rook */
    "a8a7",
    /* Knight */
    "g8h6",
    /* Move Bishop to allow short castle */
    "f8e7",

    Castle(colour, Short))
  def getMove: Move = {
    val m :: ms = moves
    moves = ms
    m
  }
}