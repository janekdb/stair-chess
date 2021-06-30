package chess.ui
import chess.model.{BoardChanged, BoardChangedSubscriber, Castled, Colour, ConfigurationChangedSubscriber, ConfigurationView, Drawn, GameChanged, GameChangedSubscriber, Piece, PieceMoved, PieceMovedCapturing, PiecePlaced, Placed, Position, Promoted, Resigned, Won}
import chess.util.TODO

class BoardAdapter(val board: Board)
    extends BoardChangedSubscriber
    with ConfigurationChangedSubscriber
    with GameChangedSubscriber {

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
      case _ => TODO.throwRuntimeEx("Unhandled case: " + event)
    }
  }

  def onBoardChanged(events: List[BoardChanged]): Unit = {
    events foreach onBoardChanged
  }

  private def setPiece(p: Position, piece: String): Unit = board.setPiece(p.getCol, p.getRow, piece)

  private def onBoardChanged(event: BoardChanged): Unit = {

    def clearSquare(p: Position): Unit = board.clearSquare(p.getCol, p.getRow)

    event match {

      /* Assume the consumer of BoardChangeEvent has access to the board configuration. */
      case PieceMoved(start, end) =>
        val Placed(colour, piece, _) = configuration.getExistingPiece(end)
        clearSquare(start)
        setPiece(end, makeLabel(colour, piece))
      case PieceMovedCapturing(start, end, captured) =>
        val Placed(colour, piece, _) = configuration.getExistingPiece(end)
        clearSquare(captured)
        clearSquare(start)
        setPiece(end, makeLabel(colour, piece))
      case Promoted(position, _) =>
        val Placed(colour, piece, _) = configuration.getExistingPiece(position)
        val promotedLabel      = makeLabel(colour, piece)
        setPiece(position, promotedLabel)
      case Castled(king, rook) =>
        for (pm <- List(king, rook)) {
          val Placed(colour, piece, _) = configuration.getExistingPiece(pm.end)
          clearSquare(pm.start)
          setPiece(pm.end, makeLabel(colour, piece))
        }
      case Resigned(colour) =>
        // TODO: UI: Improve resignation visualisation with a popup dialog
        throw new RuntimeException(s"$colour has resigned")
      case _ => TODO.throwRuntimeEx("Unhandled case: " + event)
    }
  }

  def onPiecePlaced(event: PiecePlaced): Unit = {
    setPiece(event.position, makeLabel(event.colour, event.piece))
  }

  private def makeLabel(colour: Colour, piece: Piece): String = {
    val ps = piece.toString
    (colour.toString + "-" + ps).toLowerCase
  }

  def close(): Unit = {
    board.close()
  }
}
