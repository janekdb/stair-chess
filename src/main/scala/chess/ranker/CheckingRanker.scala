package chess.ranker
import chess.model.Move
import chess.model.ConfigurationView
import chess.model.Configuration
import chess.model.Colour
import chess.model.MoveExplorer

/**
 * A ranker that ranks checking moves highest.
 */
class CheckingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {

  private def checkingMove(confView: ConfigurationView, move: Move): Boolean =
    explorerFactory(confView.applied(move)).kingInCheck(colour.opposite)

  private def rank(confView: ConfigurationView)(move: Move): Int =
    if (checkingMove(confView, move)) RANKING_HIGH else RANKING_LOW

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] =
    ranker.rankAsList(moves, rank(conf))

}