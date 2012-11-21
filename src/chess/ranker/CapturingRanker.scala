package chess.ranker
import chess.model.Colour
import chess.model.MoveExplorer
import chess.model.ConfigurationView
import chess.model.Move
import chess.model.Capturing

// TODO: Mixin a piece value source to influence capturing choice
/**
 * An implementation of MoveRanker that ranks capturing moves higher than other moves.
 */
class CapturingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {

  private def rank(move: Move) = move match { case _: Capturing => RANKING_HIGH case default => RANKING_LOW }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = ranker.rankAsList(moves, rank)

}