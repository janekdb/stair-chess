package chess.model

/** Read only access to a Configuration
  */
trait ConfigurationView {

  /** Return the last move or None
    */
  def getLastMove: Option[(Piece, Position, Position)]

  def getRows: List[List[(Colour, Piece)]]

  /** Throw exception if there is no piece at the given position. Includes the previous position if any. */
  def getExistingPiece(position: Position): (Colour, Piece, Option[Position])

  /** Include previous position option. */
  def getPiece(position: Position): Option[(Colour, Piece, Option[Position])]

  /** Return positions of all pieces of the given colour and type. */
  def locatePieces(colour: Colour, piece: Piece): List[Position]

  /** Return positions of all pieces of the given colour. */
  def locatePieces(colour: Colour): List[Position]

  /** @return true if a piece exists at the given location */
  def exists(p: Position): Boolean = getPiece(p).isDefined

  /** @return true if a piece exists at the given location with the given colour */
  // TODO: Do this in a monadic style
  def exists(p: Position, c: Colour): Boolean = {
    getPiece(p) match {
      case Some((colour, _, _)) => colour == c
      case None                 => false
    }
  }

  // TODO: Consider using apply instead of applied in ConfigurationView
  def applied(move: Move): ConfigurationView
}

object ConfigurationView {

  import chess.model.Colours.{Black, White}

  private val symbols =
    Map[Piece, String](Rook -> "R", Knight -> "N", Bishop -> "B", King -> "K", Queen -> "Q", Pawn -> "P")

  private def colourise(c: Colour)(symbol: String): String = {
    c match {
      case White => symbol
      case Black => symbol.toLowerCase
    }
  }

  // TODO: Remove mutable variables from getTextRepresentation
  /*  */
  /** Return a text representation formatted as a grid with row and column labels. Black starts at the top and is
    * represented by lowercase letters. <pre> abcdefgh 8 rnbqkbnr 7 ·p·pp·pp 6 ··p····· 5 ·····p·· 4 ·p·····P 3 N··P····
    * 2 P·P·PPP· 1 R·BQKBNR abcdefgh </pre>
    */
  def getTextRepresentation(confView: ConfigurationView): List[String] = {
    var lines: List[String] = List()
    val COLUMN_LABELS       = "  abcdefgh"
    lines ::= COLUMN_LABELS
    val rows   = confView.getRows.reverse
    var rowNum = rows.size
    for (row <- rows) {
      var line = ""
      line = line + rowNum + " "
      rowNum = rowNum - 1
      for (square <- row) {
        val symbol =
          square match {
            /* Middle dot: U+00B7 */
            case null => "·"
            case (c: Colour, p: Piece) =>
              val col = colourise(c) _
              symbols.get(p) match {
                case Some(s) => col(s)
                case None    => assert(false)
              }
            case _ => "?"
          }
        line = line + symbol
      }
      lines ::= line
    }
    lines ::= COLUMN_LABELS
    lines.reverse
  }

}
