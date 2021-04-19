package chess.ui
import java.util.concurrent.TimeUnit
import chess.model.BoardChanged
import chess.model.BoardChangedSubscriber
import chess.model.GameChanged
import chess.model.GameChangedSubscriber
import chess.model.Castled
import chess.model.Colour
import chess.model.ConfigurationChangedSubscriber
import chess.model.ConfigurationView
import chess.model.Piece
import chess.model.PieceMoved
import chess.model.PieceMovedCapturing
import chess.model.PiecePlaced
import chess.model.Position
import chess.model.Promoted
import chess.model.Resigned
import chess.model.GameOutcomeModes
import chess.util.TODO
import chess.model.Won
import chess.model.Drawn

class BoardAdapter(val board: Board) extends BoardChangedSubscriber with ConfigurationChangedSubscriber with GameChangedSubscriber {

  var configuration: ConfigurationView = _

  def onConfigurationChanged(event: ConfigurationView): Unit = {
    this.configuration = event
  }

  def onGameChanged(event: GameChanged): Unit = {
    event match {
      case Won(colour, winMode) =>
        board.showWon(colour.toString, winMode.toString)
      case Drawn(drawMode) =>
        board.showDrawn(drawMode.toString)
      case default => TODO.throwRuntimeEx("Unhandled case: " + event)
    }
  }

  def onBoardChanged(events: List[BoardChanged]): Unit = {
    events foreach onBoardChanged
  }

  private def setPiece(p: Position, piece: String) = board.setPiece(p.getCol, p.getRow, piece)

  private def onBoardChanged(event: BoardChanged): Unit = {

    def clearSquare(p: Position) = board.clearSquare(p.getCol, p.getRow)

    event match {

      /* Assume the consumer of BoardChangeEvent has access to the board configuration. */
      case PieceMoved(start, end) =>
        val (colour, piece, _) = configuration.getExistingPiece(end)
        clearSquare(start)
        setPiece(end, makeLabel(colour, piece))
      case PieceMovedCapturing(start, end, captured) =>
        val (colour, piece, _) = configuration.getExistingPiece(end)
        clearSquare(captured)
        clearSquare(start)
        setPiece(end, makeLabel(colour, piece))
      case Promoted(position, piece) =>
        val (colour, piece, _) = configuration.getExistingPiece(position)
        val promotedLabel = makeLabel(colour, piece)
        setPiece(position, promotedLabel)
      case Castled(king, rook) =>
        for (pm <- List(king, rook)) {
          val (colour, piece, _) = configuration.getExistingPiece(pm.end)
          clearSquare(pm.start)
          setPiece(pm.end, makeLabel(colour, piece))
        }
      case Resigned(colour) =>
        // TODO: UI: Improve resignation visualisation with a popup dialog
        throw new RuntimeException(s"$colour has resigned")
      case default => TODO.throwRuntimeEx("Unhandled case: " + event)
    }
  }

  def onPiecePlaced(event: PiecePlaced): Unit = {
    setPiece(event.position, makeLabel(event.colour, event.piece))
  }

  private def makeLabel(colour: Colour, piece: Piece): String = {
    val ps = piece.toString
    (colour.toString + "-" + ps).toLowerCase
  }

  def close: Unit = {
    board.close()
  }
}