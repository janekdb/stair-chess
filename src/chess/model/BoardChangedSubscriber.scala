package chess.model

trait BoardChangedSubscriber {

  def onBoardChanged(event: BoardChanged)
}