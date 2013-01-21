package chess.model

// TODO: Extend MoveParser trait
object StandardMoveParser {

  /**
   * @param moves The available moves
   * @param moveText The text to use to select a corresponding move
   * @return A Some(Move) if the text corresponds to a legal move otherwise None.
   */
  def parse(moves: List[Move], moveText: String): Option[Move] = {
    println("parse: moves: " + moves)
    val move: Option[Move] = moveText match {
      case StartEnd(start, end) => {
        println("StartEnd: " + start + end)
        /* Look for a SimpleMove matching the start and end positions */
        val sms = moves.filter { case m: SimpleMove => m.start == start && m.end == end }
        println("SimpleMoves: " + sms)
        if (sms.nonEmpty) Some(sms.head) else None

      }
    }
    move
  }
}

object StartEnd {

  def unapply(text: String): Option[(Position, Position)] =
    if (text.length == 4) Some(new Position(text.substring(0, 2)), new Position(text.substring(2, 4))) else None
}
