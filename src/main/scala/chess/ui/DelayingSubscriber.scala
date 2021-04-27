package chess.ui
import chess.model.BoardChangedSubscriber
import chess.model.BoardChanged
import chess.model.GameChanged
import chess.model.GameChangedSubscriber
import java.util.concurrent.TimeUnit
import chess.model.Drawn
import chess.model.Won
import chess.model.PieceMoved
import chess.model.Promoted
import chess.model.PieceMovedCapturing
import chess.model.Resigned
import chess.model.PiecePlaced
import chess.model.Castled

/** A subscriber that slows the display */
class DelayingSubscriber extends BoardChangedSubscriber with GameChangedSubscriber {

  val DELAY_FACTOR = 1

  private def delay(d: Int): Unit = { TimeUnit.MILLISECONDS.sleep(d / DELAY_FACTOR) }

  def onGameChanged(event: GameChanged): Unit = {
    val delayFor =
      event match {
        case _: Won => 10000
        case _: Drawn => 100
        case _ =>
          assert(assertion = false, "Unhandled case: " + event)
          /* Without this delayFor typed as AnyVal */
          0
      }
    delay(delayFor)
  }

  def onBoardChanged(events: List[BoardChanged]): Unit = {
    for (e <- events) onBoardChanged(e)
  }

  private def onBoardChanged(event: BoardChanged): Unit = {
    val delayFor =
      event match {
        case _: PieceMoved => 1000
        case _: PieceMovedCapturing => 1
        case _: Promoted => 100
        case _: Castled => 100
        case _: Resigned => 1000
        case _ =>
          assert(assertion = false, "Unhandled case: " + event)
          /* Without this delayFor typed as AnyVal */
          0
      }
    delay(delayFor)
  }

  def onPiecePlaced(event: PiecePlaced): Unit = {
    delay(1)
  }

}