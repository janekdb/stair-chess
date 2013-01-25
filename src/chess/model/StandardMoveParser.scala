package chess.model

// TODO: Parse resign
// TODO: Extend MoveParser trait
object StandardMoveParser {

  // TODO: Restrict the visibility of pieces
  val pieces = Map("rook" -> Rook(), "knight" -> Knight(), "bishop" -> Bishop(), "queen" -> Queen())

  /**
   * @param moves The available moves
   * @param moveText The text to use to select a corresponding move, for example "e7-queen", "e7e5", "castle-short"
   * @return A Some(Move) if the text corresponds to a legal move otherwise None.
   */
  def parse(moves: List[Move], moveText: String): Option[Move] = {
    val move: Option[Move] = moveText match {
      case StartEnd(start, end) => {
        moves find { case m: SimpleMove => (m.start, m.end) == (start, end) case default => false }
      }
      case LongShort(castlingType) => {
        moves find { case m: Castle => m.castlingType == castlingType case default => false }
      }
      case StartPiece(start, piece) => {
        moves find { case m: Promote => (m.start, m.piece) == (start, piece) case default => false }
      }
      case StartEndPiece(start, end, piece) => {
        moves find { case m: PromoteCapturing => (m.start, m.end, m.piece) == (start, end, piece) case default => false }
      }
      case default => None
    }
    move
  }
}

private object StartEnd {
  def unapply(text: String): Option[(Position, Position)] =
    if (text.length == 4) Some(new Position(text.substring(0, 2)), new Position(text.substring(2, 4))) else None
}

private object LongShort {
  private val inputMap = Map("castle-long" -> Long, "castle-short" -> Short)
  def unapply(text: String): Option[CastlingType] = inputMap.get(text)
}

import scala.util.matching.Regex

private object StartPiece {
  import StandardMoveParser.pieces
  // TODO: Remove assumptions about board size from regular expression
  val pattern = new Regex("^([a-h][1-8])-([a-z]+)$", "start", "piece")
  def unapply(text: String): Option[(Position, Piece)] = {
    pattern findFirstIn text match {
      case Some(pattern(start, piece)) => pieces.get(piece) map { p => (new Position(start), p) }
      case default => None
    }
  }
}

private object StartEndPiece {
  import StandardMoveParser.pieces
  // TODO: Remove assumptions about board size from regular expression
  val pattern = new Regex("^([a-h][1-8])([a-h][1-8])-([a-z]+)$", "start", "end", "piece")
  def unapply(text: String): Option[(Position, Position, Piece)] = {
    pattern findFirstIn text match {
      case Some(pattern(start, end, piece)) => pieces.get(piece) map { p => (new Position(start), new Position(end), p) }
      case default => None
    }
  }
}
