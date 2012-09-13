package chess.player
import chess.model.Move
import chess.model.ConfigurationView

trait MoveRanker {
  /**
   * @param A non-null list of legal moves.
   * @return A possibly empty list of non empty move lists where the first move list contains the most preferred moves.
   */
  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]]
}