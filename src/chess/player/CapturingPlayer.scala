package chess.player
import chess.model.Colour
import chess.model.Move
import chess.model.MoveExplorer
import chess.model.Capturing
import chess.model.Configuration

// TODO: Mixin a piece value source to influence capturing player
// TODO: Replace CapturingPlayer with CapturingRanker in combination with ShellPlayer
/**
 * A player that selects at random preferring capturing moves.
 */
class CapturingPlayer(val colour: Colour, val explorer: MoveExplorer) extends Player {

  def getName = "CapturingPlayer"

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