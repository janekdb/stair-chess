package chess.model

import ex.IllegalPromotionException

/** Verbs */

abstract class Move

/* Assume the consumer of MovePiece has access to the board configuration. */
case class MovePiece(start: Position, end: Position) extends Move {
  def this(move: String) = this(new Position(move.substring(0,2)), new Position(move.substring(2,4)))
}
case class Resign(colour: Colour) extends Move
case class Castle(colour: Colour, castlingType: CastlingType) extends Move
case class Promote(start: Position, end: Position, piece: Piece) extends Move {
  def this(move: String, piece: Piece) = {
    this(new Position(move.substring(0,2)), new Position(move.substring(2,4)), piece)
    if(List(King(), Pawn()) contains piece){
      throw new IllegalPromotionException(piece)
    }
  }
}
case class EnPassant(start: Position, end: Position) extends Move {
  val taken = new Position(end.getCol, start.getRow)
}
