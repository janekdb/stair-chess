package chess.ranker
import chess.model.Colour
import chess.model.MoveExplorer
import chess.model.ConfigurationView
import chess.model.Move
import chess.model.Capturing

/**
 * An implementation of MoveRanker that ranks capturing moves higher than other moves.
 */
class CapturingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {

  private def rank(move: Move) = move match { case _: Capturing => 0 case default => 1 }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] =
    // TODO: Refactor this move partitioning code into a utility function
    moves.groupBy(rank).toList.sortBy(_._1).map(_._2)

}