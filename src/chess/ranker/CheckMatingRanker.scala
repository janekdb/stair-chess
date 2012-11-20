package chess.ranker
import chess.model.Colour
import chess.model.Move
import chess.model.MoveExplorer
import chess.model.ConfigurationView

/**
 * An implementation of MoveRanker that ranks check mating moves higher than other moves.
 */
class CheckMatingRanker(val explorerFactory: ConfigurationView => MoveExplorer, colour: Colour) extends MoveRanker {

  // TODO: Add HIGH and LOW constants to MoveRanker to avoid numeric literals. */
  /** @return 1 if the move leads to checkmate otherwise 0 */
  private def rank(confView: ConfigurationView)(move: Move): Int = {
    val future = confView.applied(move)
    val e = explorerFactory(future)
    if (checkForCheckMate(e, colour.opposite, future)) 1 else 0
  }

  // TODO: Consolidate the checkmate testing code with CheckMatingRanker
  private def checkForCheckMate(moveExplorer: MoveExplorer, colour: Colour, conf: ConfigurationView): Boolean = {
    moveExplorer.kingInCheck(colour) && !checkedKingCanEscape(colour, conf)
  }

  /*
   * @return true when there is at least one move that will get the king out of
   * check
   */
  private def checkedKingCanEscape(colour: Colour, conf: ConfigurationView): Boolean = {
    val me = explorerFactory(conf)
    me.legalMoves(colour).nonEmpty
  }

  def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = ranker.rankAsList(moves, rank(conf))
}
