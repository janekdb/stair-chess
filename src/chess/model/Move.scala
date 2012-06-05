package chess.model

import ex.IllegalPromotionException

private object Utils {
  def start(move: String): Position = new Position(move.substring(0, 2))
  def end(move: String): Position = new Position(move.substring(2, 4))
  def rejectInvalidPromotionPiece(piece: Piece) {
    if (List(King(), Pawn()) contains piece)
      throw new IllegalPromotionException(piece)
  }
}

import Utils._

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
  def this(move: String) = this(start(move), end(move))
}
case class MovePieceCapturing(val start: Position, val end: Position) extends SimpleMove {
  require(start != end, "Start must be different to end: " + start)
  def this(move: String) = this(start(move), end(move))
}
case class Resign(colour: Colour) extends Move
case class Castle(colour: Colour, castlingType: CastlingType) extends Move
//case class PromoteCapturing(val start: Position, val end: Position, val piece: Piece) extends SimpleMove {
case class Promote(val start: Position, val piece: Piece) extends SimpleMove {
  require(start != end, "Start must be different to end: " + start)
  //TODO: Remove magic value of 7
  val end = { val row = if (start.row == 7) start.row + 1 else start.row - 1; new Position(start.col, row) }
  def this(move: String, piece: Piece) = {
    this(start(move), piece)
    rejectInvalidPromotionPiece(piece)
  }
}
case class PromoteCapturing(val start: Position, val end: Position, val piece: Piece) extends SimpleMove {
  require(start != end, "Start must be different to end: " + start)
  def this(move: String, piece: Piece) = {
    this(start(move), end(move), piece)
    rejectInvalidPromotionPiece(piece)
  }
}
case class EnPassant(val start: Position, val end: Position) extends SimpleMove {
  require(start != end, "Start must be different to end: " + start)
  val captured = new Position(end.getCol, start.getRow)
}
