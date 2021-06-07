package chess.ranker

import chess.model.{Colour, ConfigurationView, Move, MoveExplorer}

/** A MoverRanker that ranks moves forking two or move pieces higher than other moves.
  */
class ForkingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {
  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = moves :: Nil
}
//class CapturingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {
//
//  private def rank(move: Move): Int = move match { case _: Capturing => RANKING_HIGH case _ => RANKING_LOW }
//
//  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = ranker.rankAsList(moves, rank)
//
//}
