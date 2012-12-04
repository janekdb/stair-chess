package chess.player
import chess.model.Colour
import chess.model.Configuration
import chess.model.ConfigurationView
import chess.model.Move
import chess.model.MoveExplorer
import chess.player.Player
import chess.ranker.MoveRanker

/**
 * A class that selects a move from a ranked list of move lists.
 */
class ShellPlayer(val name: String, val colour: Colour, explorerFactory: ConfigurationView => MoveExplorer, val moveRanker: MoveRanker) extends Player {

  def getMove(configuration: Configuration): Option[Move] = {
    val moves = explorerFactory(configuration).legalMoves(colour)
    val rankedMoves = moveRanker.rankMoves(moves, configuration)
    if (rankedMoves.isEmpty) {
      None
    } else {
      val ms = rankedMoves.head
      Some(ms(new util.Random().nextInt(ms.size)))
    }
  }
  
  def getName: String = name

}