package chess.model

// TODO: Move method into more logical class, trait or object

object Misc {

  def kingInCheck(colour: Colour, conf: Configuration): Boolean = {
    val futureMoveExplorer = new StandardMoveExplorer(conf)
    val List(king) = conf.locatePieces(colour, King())
    // TODO: Consolidate this code with the same code as the Castling case by passing a block to
    //   code that finds and iterates over the opponents pieces.
    // TODO: Confirm this considers non-basic move restriction such as en-passant.
    val opponentPositions = conf.locatePieces(colour.opposite)
    opponentPositions.exists(p =>
      futureMoveExplorer.getBasicPositions(p).contains(king))
  }

}