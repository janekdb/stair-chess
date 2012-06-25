package chess.player
import chess.model.Colour
import chess.model.Move
import chess.model.MoveExplorer
import chess.model.Capturing
import chess.model.Configuration

/**
 * A player that selects at random preferring capturing moves.
 */
class CapturingPlayer(val colour: Colour, val explorer: MoveExplorer) extends Player {

  def getMove(configuration: Configuration): Option[Move] = {
    val moves = explorer.legalMoves(colour)
    if (moves.isEmpty) {
      None
    } else {
      val capturing = moves filter { case c: Capturing => true case default => false }
      val ms = if (capturing.nonEmpty) capturing else moves
      Some(ms(new util.Random().nextInt(ms.size)))
    }
  }
}