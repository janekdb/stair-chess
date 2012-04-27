package chess.library

import chess.model.{ BoardModel, Castle, Colour, Move, MovePiece, Position, Promote, Resign, Short, Long, Queen }
import chess.model.Promote

object Library {

  implicit def stringToMovePiece(s: String) = new MovePiece(s)

  // TODO: Use this library somewhere
  var scholarsMateBlack: List[Move] = List(
    "e7e5",
    "b8c6",
    "g8f6")

  var scholarsMateWhite: List[Move] = List(
    "e2e4",
    "d1h5",
    "f1c4",
    "h5f7")

  val scholarsMate = new Game(scholarsMateWhite, scholarsMateBlack)

}

class Game(whiteMoves: List[Move], blackMoves: List[Move])
