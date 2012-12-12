package chess.ranker
import chess.model.Colour
import chess.model.Move
import chess.model.MoveExplorer
import chess.model.ConfigurationView

/**
 * An implementation of MoveRanker that ranks check mating moves higher than other moves.
 */
class CheckMatingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {

  private def rank(confView: ConfigurationView)(move: Move): Int = {
    val future = confView.applied(move)
    val e = explorerFactory(future)
    if (e.kingInCheckMate(colour.opposite)) RANKING_HIGH else RANKING_LOW
  }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = ranker.rankAsList(moves, rank(conf))
}
