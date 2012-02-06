package chess.model

/**
 * Classes that can encapsulate moves extend this trait.
 */
trait MoveExplorer {

  def getBasicPositions(position: Position): Set[Position]

  /** @throw IllegalMoveException when the move would violate a condition not considered by {@link getBasicPositions} */
  def rejectIllegalMove(move: Move)

}