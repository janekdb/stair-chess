package chess.model

// TODO: Move method into more logical class, trait or object

object Misc {

  def kingInCheck(colour: Colour, conf: Configuration): Boolean = {
    val futureMoveExplorer = new StandardMoveExplorer(conf)
    val List(king) = conf.locatePieces(colour, King())
    // TODO: Consolidate this code with the same code as the Castling case by passing a block to
    //   code that finds and iterates over the opponents pieces.
    val opponentPositions = conf.locatePieces(colour.opposite)
    opponentPositions.foreach { p =>
      val attackedPositions = futureMoveExplorer.getBasicPositions(p)
      if (attackedPositions.contains(king)) {
        return true
      }
    }
    return false
  }

}