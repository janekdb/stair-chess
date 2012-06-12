package chess.model

/**
 * Read only access to a Configuration
 */
trait ConfigurationView {

  /**
   * Return the last move or None
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
  def exists(p: Position) = getPiece(p).isDefined

  /** @return true if a piece exists at the given location with the given colour*/
  // TODO: Do this in a monadic style
  def exists(p: Position, c: Colour) = {
    getPiece(p) match {
      case Some((colour, _, _)) => colour == c
      case None => false
    }
  }

}