package chess.model

// TODO: Move method into more logical class, trait or object

object Misc {
  def kingInCheck(colour: Colour, explorer: MoveExplorer): Boolean = {
    val List(king) = explorer.locatePieces(colour, King())
    // TODO: Consolidate this code with the same code as the Castling case by passing a block to
    //   code that finds and iterates over the opponents pieces.
    // TODO: Confirm this considers non-basic move restriction such as en-passant.
    val opponentPositions = explorer.locatePieces(colour.opposite)
    opponentPositions.exists(p =>
      explorer.getBasicPositions(p) contains king)
  }

}