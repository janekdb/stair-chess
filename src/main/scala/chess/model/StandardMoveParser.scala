package chess.model

// TODO: Extend MoveParser trait
object StandardMoveParser {

  // TODO: Restrict the visibility of pieces
  val pieces = Map("rook" -> Rook, "knight" -> Knight, "bishop" -> Bishop, "queen" -> Queen)

  /** @param moves
    *   The available moves
    * @param moveText
    *   The text to use to select a corresponding move, for example "e7-queen", "e7e5", "castle-short"
    * @return
    *   A Some(Move) if the text corresponds to a legal move otherwise None.
    */
  def parse(moves: List[Move], moveText: String): Option[Move] = {

    val move: Option[Move] = moveText match {
      case StartEnd(start, end) =>
        moves find {
          case m: SimpleMove => (m.start, m.end) == (start, end)
          case _             => false
        }
      case LongShort(castlingType) =>
        moves find {
          case m: Castle => m.castlingType == castlingType
          case _         => false
        }
      case StartPiece(start, piece) =>
        moves find {
          case m: Promote => (m.start, m.piece) == (start, piece)
          case _          => false
        }
      case StartEndPiece(start, end, piece) =>
        moves find {
          case m: PromoteCapturing => (m.start, m.end, m.piece) == (start, end, piece)
          case _                   => false
        }
      case _ =>
        // TODO: Report this via a listener interface
        println("No match for: '" + moveText + "'")
        None
    }
    move
  }
}

import scala.util.matching.Regex

private object StartEnd {
  // TODO: Remove assumptions about board size from regular expression
  val pattern = new Regex("^([a-h][1-8])([a-h][1-8])$", "start", "end")
  def unapply(text: String): Option[(Position, Position)] =
    pattern findFirstIn text match {
      case Some(pattern(start, end)) => Some(new Position(start), new Position(end))
      case _                         => None
    }
}

private object LongShort {
  private val inputMap                            = Map("castle-long" -> Long, "castle-short" -> Short)
  def unapply(text: String): Option[CastlingType] = inputMap.get(text)
}

private object StartPiece {
  import StandardMoveParser.pieces
  // TODO: Remove assumptions about board size from regular expression
  val pattern = new Regex("^([a-h][1-8])-([a-z]+)$", "start", "piece")
  def unapply(text: String): Option[(Position, Piece)] =
    pattern findFirstIn text match {
      case Some(pattern(start, piece)) => pieces.get(piece) map { p => (new Position(start), p) }
      case _                           => None
    }
}

private object StartEndPiece {
  import StandardMoveParser.pieces
  // TODO: Remove assumptions about board size from regular expression
  val pattern = new Regex("^([a-h][1-8])([a-h][1-8])-([a-z]+)$", "start", "end", "piece")
  def unapply(text: String): Option[(Position, Position, Piece)] =
    pattern findFirstIn text match {
      case Some(pattern(start, end, piece)) =>
        pieces.get(piece) map { p => (new Position(start), new Position(end), p) }
      case _ => None
    }
}
