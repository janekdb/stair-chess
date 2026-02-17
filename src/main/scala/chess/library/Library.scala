package chess.library

import chess.model.{Move, MovePiece}
import chess.model.MovePieceCapturing

object Library {

  given Conversion[String, MovePiece] = new MovePiece(_)

  private val scholarsMateBlack: List[Move] = List("e7e5", "b8c6", "g8f6")

  private val scholarsMateWhite: List[Move] = List("e2e4", "d1h5", "f1c4", new MovePieceCapturing("h5f7"))

  val scholarsMate = new Game(scholarsMateWhite, scholarsMateBlack)

}

class Game(val whiteMoves: List[Move], val blackMoves: List[Move])
