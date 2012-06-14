package chess.player
import chess.model.Colour
import chess.model.Move
import chess.model.MoveExplorer
import chess.model.Capturing

/**
 * A player that selects at random preferring capturing moves.
 */
class CapturingPlayer(val colour: Colour, val explorer: MoveExplorer) extends Player {

  def getMove: Move = {
    val moves = explorer.legalMoves(colour)
    // TODO: Handle stalemate in a common player superclass
    if (moves.isEmpty) throw new RuntimeException("No move found")
    val capturing = moves filter { case c: Capturing => true case default => false }
    val ms = if(capturing.nonEmpty) capturing else moves
    ms(new util.Random().nextInt(ms.size))
  }
}