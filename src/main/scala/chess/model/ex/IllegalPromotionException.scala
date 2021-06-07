package chess.model.ex

import chess.model.Piece

class IllegalPromotionException(piece: Piece) extends RuntimeException("Illegal promotion: " + piece) {

  override def toString(): String = {
    "Illegal promotion: " + piece
  }
}
