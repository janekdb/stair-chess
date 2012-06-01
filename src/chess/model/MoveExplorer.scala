package chess.model

/**
 * Classes that can encapsulate moves extend this trait.
 */
trait MoveExplorer {

  // TODO: Consider make private if legalMoves is sufficient for clients
  def getBasicPositions(position: Position): Set[Position]

  /** @throw IllegalMoveException when the move would violate a condition not considered by {@link getBasicPositions} */
  def rejectIllegalMove(move: Move)

  /** @return true if the king of the selected colour is in check */
  def kingInCheck(colour: Colour): Boolean

  /** @return All legal moves. */
  def legalMoves(colour: Colour): List[Move]
}