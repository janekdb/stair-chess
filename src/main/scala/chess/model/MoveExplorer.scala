package chess.model

/**
 * Classes that can encapsulate moves extend this trait.
 */
trait MoveExplorer {

  /** @throw IllegalMoveException when the move would violate a condition not considered by {@link getBasicPositions} */
  def rejectIllegalMove(move: Move): Unit

  /** @return true if the king of the selected colour is in check */
  def kingInCheck(colour: Colour): Boolean

  /** @return true if the king is in check and cannot escape from check */
  def kingInCheckMate(colour: Colour): Boolean

  /** @return All legal moves. */
  def legalMoves(colour: Colour): List[Move]
}