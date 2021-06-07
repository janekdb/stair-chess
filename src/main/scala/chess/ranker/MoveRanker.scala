package chess.ranker
import chess.model.Move
import chess.model.ConfigurationView

trait MoveRanker {

  /** Convenience values to use when ranking moves into two groups. */
  val RANKING_HIGH = 1
  val RANKING_LOW  = 0

  /** @param moves
    *   A non-null list of legal moves.
    * @return
    *   A possibly empty list of non empty move lists where the first move list contains the most preferred moves.
    */
  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]]
}
