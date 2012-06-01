package chess.model

import ex.IllegalPromotionException


//class StringOps(val s: String) {
//  def start = new Position(s.substring(0, 2))
//  def end = new Position(s.substring(2, 4))
//}
//implicit def stringOps(s: String) = new StringOps(s)

/** Verbs */

abstract class Move

/* A simple move has start and end positions */
abstract class SimpleMove extends Move {
  def start: Position
  def end: Position
}

/* Assume the consumer of MovePiece has access to the board configuration. */
case class MovePiece(val start: Position, val end: Position) extends SimpleMove {
  require(start != end, "Start must be different to end: " + start)
  // TODO: Add methods to move.{start, end} with an implicit operator
  // TODO: Determine if an extractor could use use to parse "a1a2" into an instance of MovePiece
  def this(move: String) = this(new Position(move.substring(0, 2)), new Position(move.substring(2, 4)))
}
case class MovePieceCapturing(val start: Position, val end: Position) extends SimpleMove {
  require(start != end, "Start must be different to end: " + start)
  // TODO: Add methods to move.{start, end} with an implicit operator
  // TODO: Determine if an extractor could use use to parse "a1a2" into an instance of MovePiece
  def this(move: String) = this(new Position(move.substring(0, 2)), new Position(move.substring(2, 4)))
}
case class Resign(colour: Colour) extends Move
case class Castle(colour: Colour, castlingType: CastlingType) extends Move
case class Promote(val start: Position, val end: Position, val piece: Piece) extends SimpleMove {
  // TODO: Remove end because for promotion without capturing the start determines the end
  def this(move: String, piece: Piece) = {
    // TODO: Add methods to move.{start, end} with an implicit operator
    // TODO: Determine if an extractor could use use to parse "a1a2" into an instance of MovePiece
    this(new Position(move.substring(0, 2)), new Position(move.substring(2, 4)), piece)
    if (List(King(), Pawn()) contains piece) {
      throw new IllegalPromotionException(piece)
    }
  }
}
case class PromoteCapturing(val start: Position, val end: Position, val piece: Piece) extends SimpleMove {
  // TODO: Reduce duplication with Promote
  def this(move: String, piece: Piece) = {
    // TODO: Add methods to move.{start, end} with an implicit operator
    // TODO: Determine if an extractor could use use to parse "a1a2" into an instance of MovePiece
    this(new Position(move.substring(0, 2)), new Position(move.substring(2, 4)), piece)
    if (List(King(), Pawn()) contains piece) {
      throw new IllegalPromotionException(piece)
    }
  }
}
case class EnPassant(val start: Position, val end: Position) extends SimpleMove {
  val captured = new Position(end.getCol, start.getRow)
}
