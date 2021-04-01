package chess.ranker
import chess.model.Move
import chess.model.ConfigurationView
import chess.model.Colour
import chess.model.MoveExplorer
import chess.model.Capturing
import chess.model.MovePiece

/**
 * An implementation of MoveRanker that ranks moves that reduce the number of attacked pieces higher than other moves.
 */
class CaptureEvadingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {

  /*
   * The fewer piece are under attack after the move the higher the rank is.
   * @return The negation of the number of pieces under attack.
   */
  private def rank(confView: ConfigurationView)(move: Move): Int = {
    val future = confView.applied(move)
    val e = explorerFactory(future)
    val legalMoves = e.legalMoves(colour.opposite)
    val capturingMoves = legalMoves filter { case _: Capturing => true case default => false }
    -capturingMoves.size
  }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = ranker.rankAsList(moves, rank(conf))

}