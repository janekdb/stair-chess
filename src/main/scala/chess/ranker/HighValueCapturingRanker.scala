package chess.ranker

import chess.model.{Colour, ConfigurationView, Move, MoveExplorer, MovePieceCapturing}
import chess.ranker

/** A ranker which prefers capturing high value pieces
  */
class HighValueCapturingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour)
    extends MoveRanker {
  private def rank(confView: ConfigurationView)(move: Move): Int = {
    move match {
      case m: MovePieceCapturing =>
        val (_, capturedPiece, _) = confView.getExistingPiece(m.end)
        RANKING_LOW + capturedPiece.value
      case _ => RANKING_LOW
    }
  }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = ranker.rankAsList(moves, rank(conf))
}
