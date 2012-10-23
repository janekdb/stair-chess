package chess.player

import chess.model.ConfigurationView
import scala.collection.immutable.List
import chess.util.TODO
import chess.model.Move
import chess.ranker.MoveRanker

/**
 * A class which ranks moves by successively applying a list of MoveRankers. Moves are ranked
 * by the given rankers following the order of the primary constructor params. In essence this is a
 * multi-key sort operation where the rankers determine the sort key for a move.
 */
class ChainedMoveRanker(val moveRankers: MoveRanker*) extends MoveRanker {

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
    iter(List(moves), moveRankers.toList, conf)
  }

  private type CV = ConfigurationView

  private type RankedMoves = List[List[Move]]

  private def iter(rankedMoves: RankedMoves, rankers: List[MoveRanker], conf: CV): RankedMoves = {
    rankers match {
      case Nil => rankedMoves
      case r :: rs => {
        val rm = rank(rankedMoves, r, conf)
        iter(rm, rs, conf)
      }
    }
  }

  private def rank(rankedMoves: RankedMoves, ranker: MoveRanker, conf: CV): RankedMoves = {
    /* Reject bad rankers that include empty move lists. */
    def bypass(moves: List[Move]): List[List[Move]] = if (moves.size == 1) List(moves) else ranker.rankMoves(moves, conf)
    for (moves <- rankedMoves; m <- bypass(moves)) yield {
      if (m.isEmpty) throw new AssertionError("Empty move list in rankedMoves: " + rankedMoves)
      m
    }
  }
}
