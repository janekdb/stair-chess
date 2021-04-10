package chess.model

trait BoardChangedSubscriber {

  /**
   * @param events The parts comprising a single move.
   */
  def onBoardChanged(events: List[BoardChanged]): Unit

  /**
   * @param event Details of the piece that was placed during the
   *              board setup.
   */
  def onPiecePlaced(event: PiecePlaced): Unit
}