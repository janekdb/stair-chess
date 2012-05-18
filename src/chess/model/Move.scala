package chess.model

import ex.IllegalPromotionException

/** Verbs */

abstract class Move

/* A simple move has start and end positions */
abstract class SimpleMove extends Move {
  def start: Position
  def end: Position
}

/* Assume the consumer of MovePiece has access to the board configuration. */
case class MovePiece(start: Position, end: Position) extends SimpleMove {
  require(start != end, "start must be different to end: " + start)
  // TODO: Determine if an extractor could use use to parse "a1a2" into an instance of MovePiece
  def this(move: String) = this(new Position(move.substring(0,2)), new Position(move.substring(2,4)))
}
case class Resign(colour: Colour) extends Move
case class Castle(colour: Colour, castlingType: CastlingType) extends Move
case class Promote(start: Position, end: Position, piece: Piece) extends SimpleMove {
  def this(move: String, piece: Piece) = {
    this(new Position(move.substring(0,2)), new Position(move.substring(2,4)), piece)
    if(List(King(), Pawn()) contains piece){
      throw new IllegalPromotionException(piece)
    }
  }
}
case class EnPassant(start: Position, end: Position) extends SimpleMove {
  val taken = new Position(end.getCol, start.getRow)
}
