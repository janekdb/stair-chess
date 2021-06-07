package chess.model

trait MoveFactory {

  // TODO: Decide if colour is needed on MoveFactory.getMove
  def getMove(colour: Colour, conf: Configuration): Option[Move]
}
