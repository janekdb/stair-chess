package chess.model

trait BoardChangedSubscriber {

  /**
   * @param events The parts comprising a single move.
   */
  def onBoardChanged(events: List[BoardChanged])
}