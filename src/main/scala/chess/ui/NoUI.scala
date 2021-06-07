package chess.ui
import chess.model.GameChangedSubscriber
import chess.model.BoardChangedSubscriber
import chess.model.GameChanged
import chess.model.BoardChanged
import chess.model.PiecePlaced

/** A UI event listener that does nothing.
  */
object NoUI extends BoardChangedSubscriber with GameChangedSubscriber {

  def onGameChanged(event: GameChanged): Unit          = ()
  def onBoardChanged(events: List[BoardChanged]): Unit = ()
  def onPiecePlaced(event: PiecePlaced): Unit          = ()

}
