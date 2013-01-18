package chess.model

// TODO: Extend MoveParser trait
object StandardMoveParser {

  /**
   * @return A Some(Move) if the text corresponds to a legal move otherwise None.
   */
  def parse(colour: Colour, conf: Configuration, moveText: String): Option[Move] = {
    assert(conf != null, "conf was null")
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(colour)
    Console.out.println("legalMoves: " + moves)
    val move: Option[Move] = moveText match {
      case StartEnd(start, end) => {
        Console.out.println("StartEnd: " + start + end)
        /* Look for a SimpleMove matching the start and end positions */
        val sms = moves.filter { case m: SimpleMove => m.start == start && m.end == end }
        Console.out.println("SimpleMoves: " + sms)
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
