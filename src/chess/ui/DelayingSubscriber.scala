package chess.ui
import chess.model.BoardChangedSubscriber
import chess.model.BoardChanged
import java.util.concurrent.TimeUnit
import chess.model.Drawn
import chess.model.Won
import chess.model.PieceMoved
import chess.model.Promoted
import chess.model.PieceMovedCapturing
import chess.util.TODO
import chess.model.Resigned
import chess.model.PiecePlaced
import chess.model.Castled

/** A subscriber that slows the display */
class DelayingSubscriber extends BoardChangedSubscriber {

  val DELAY_FACTOR = 10;

  private def delay(d: Int) { TimeUnit.MILLISECONDS.sleep(d * DELAY_FACTOR) }

  def onBoardChanged(event: BoardChanged) {
    val delayFor =
      event match {
        case _: PiecePlaced => 2
        case _: PieceMoved => 25
        case _: PieceMovedCapturing => 25
        case _: Promoted => 100
        case _: Castled => 100
        case _: Resigned => 1000
        case _: Won => 100
        case _: Drawn => 100
        case default => {
          assert(false, "Unhandled case: " + event)
          /* Without this delayFor typed as AnyVal */
          0
        }
      }
    delay(delayFor)
  }
}