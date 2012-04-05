package chess.model

/**
 * Classes that can encapsulate moves extend this trait.
 */
trait MoveExplorer {

  def getBasicPositions(position: Position): Set[Position]

  /** @throw IllegalMoveException when the move would violate a condition not considered by {@link getBasicPositions} */
  def rejectIllegalMove(move: Move)

  /** Return positions of all pieces of the given colour and type. */
  def locatePieces(colour: Colour, piece: Piece): List[Position]

  /** Return positions of all pieces of the given colour. */
  def locatePieces(colour: Colour): List[Position]

}