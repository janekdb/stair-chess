package chess.model

/**
 * Objects can encapsulate moves extend this trait.
 */
trait MoveExplorer {

  def getBasicPositions(position: Position): List[Position]

}