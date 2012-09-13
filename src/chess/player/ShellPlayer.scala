package chess.player
import chess.util.TODO
import chess.model.Move
import chess.model.Configuration
import chess.model.Colour
import chess.model.MoveExplorer

/**
 * A class that selects a move from a ranked list of move lists.
 */
class ShellPlayer(val colour: Colour, val explorer: MoveExplorer, val moveRanker: MoveRanker) extends Player {

  def getMove(configuration: Configuration): Option[Move] = {
    val moves = explorer.legalMoves(colour)
    val rankedMoves = moveRanker.rankMoves(moves, configuration)
    if (rankedMoves.isEmpty) {
      None
    } else {
      val ms = rankedMoves.head
      Some(ms(new util.Random().nextInt(ms.size)))
    }
  }
  
  def getName: String = TODO.throwRuntimeEx

}