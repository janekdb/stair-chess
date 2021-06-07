package chess.model

trait GameChangedSubscriber {

  def onGameChanged(event: GameChanged): Unit
}
