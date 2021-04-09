package chess.model

import ex.IllegalPromotionException
import Constants._

private object Utils {
  def start(move: String): Position = new Position(move.substring(0, 2))
  def end(move: String): Position = new Position(move.substring(2, 4))
  def rejectInvalidPromotionPiece(piece: Piece): Unit = {
    if (List(King, Pawn) contains piece)
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

trait Capturing

/* Assume the consumer of MovePiece has access to the board configuration. */
case class MovePiece(start: Position, end: Position) extends SimpleMove {
  require(start != end, "Start must be different to end: " + start)
  def this(move: String) = this(start(move), end(move))
}
case class MovePieceCapturing(start: Position, end: Position) extends SimpleMove with Capturing {
  require(start != end, "Start must be different to end: " + start)
  def this(move: String) = this(start(move), end(move))
}
case class Castle(colour: Colour, castlingType: CastlingType) extends Move
//case class PromoteCapturing(val start: Position, val end: Position, val piece: Piece) extends SimpleMove {
case class Promote(start: Position, piece: Piece) extends SimpleMove {
  /* Use the current row to determine the end row */
  val end = new Position(start.col, if (start.row == WHITE_HOME_ROW + 1) WHITE_HOME_ROW else BLACK_HOME_ROW)
  require(start != end, "Start must be different to end: " + start)
  def this(move: String, piece: Piece) = {
    this(start(move), piece)
    rejectInvalidPromotionPiece(piece)
  }
}
case class PromoteCapturing(start: Position, end: Position, piece: Piece) extends SimpleMove with Capturing {
  require(start != end, "Start must be different to end: " + start)
  def this(move: String, piece: Piece) = {
    this(start(move), end(move), piece)
    rejectInvalidPromotionPiece(piece)
  }
}
case class EnPassant(start: Position, end: Position) extends SimpleMove with Capturing {
  require(start != end, "Start must be different to end: " + start)
  val captured = new Position(end.getCol, start.getRow)
}
