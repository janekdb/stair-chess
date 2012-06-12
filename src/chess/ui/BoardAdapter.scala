package chess.ui
import chess.model.ConfigurationChangedSubscriber
import java.util.regex.Pattern
import chess.model.ConfigurationView
import chess.model.Won
import chess.model.PieceMoved
import chess.model.Promoted
import chess.model.PieceMovedCapturing
import chess.util.TODO
import chess.model.Resigned
import chess.model.PiecePlaced
import chess.model.BoardChanged
import chess.model.Castled
import chess.model.BoardChangedSubscriber
import chess.model.Position
import java.util.concurrent.TimeUnit

// TODO: Move BoardAdapter into the ui package
class BoardAdapter(val board: Board) extends BoardChangedSubscriber with ConfigurationChangedSubscriber {

  var configuration: ConfigurationView = null

  def onConfigurationChanged(event: ConfigurationView) {
    this.configuration = event
  }

  def onBoardChanged(event: BoardChanged) {

    def clearSquare(p: Position) = board.clearSquare(p.getCol, p.getRow)
    def setPiece(p: Position, piece: String) = board.setPiece(p.getCol, p.getRow, piece)
    val DELAY_FACTOR = 1;

    def delay(d: Int) { TimeUnit.MILLISECONDS.sleep(d * DELAY_FACTOR) }

    event match {

      /* Assume the consumer of BoardChangeEvent has access to the board configuration. */
      case PiecePlaced(colour, piece, position) => {
        setPiece(position, convertLabel(colour + "-" + piece))
        delay(2)
      }
      case PieceMoved(start, end) => {
        val (colour, piece, _) = configuration.getExistingPiece(end)
        clearSquare(start)
        setPiece(end, convertLabel(colour + "-" + piece))
        delay(100)
      }
      case PieceMovedCapturing(start, end, captured) => {
        val (colour, piece, _) = configuration.getExistingPiece(end)
        clearSquare(captured)
        clearSquare(start)
        setPiece(end, convertLabel(colour + "-" + piece))

        delay(100)
      }
      case Promoted(position, piece) => {
        val (colour, piece, _) = configuration.getExistingPiece(position)
        val promotedLabel = convertLabel(colour + "-" + piece)
        setPiece(position, promotedLabel)
        delay(100)
      }
      case Castled(king, rook) => {
        for (pm <- List(king, rook)) {
          val (colour, piece, _) = configuration.getExistingPiece(pm.end)
          clearSquare(pm.start)
          setPiece(pm.end, convertLabel(colour + "-" + piece))
        }
      }
      case Resigned(colour) => {
        // TODO: UI: Improve resignation visualisation with a popup dialog
        throw new RuntimeException(colour + " has resigned")
      }
      case Won(colour, winMode) => {
        board.showWon(colour.toString, winMode.toString)
      }

      case default => TODO.throwRuntimeEx("Unhandled case: " + event)
    }
  }

  val LABEL_PAT = Pattern.compile("(black|white)++-[a-z]++\\(\\)", Pattern.CASE_INSENSITIVE);

  // TODO: Use two parameters to allow string concatenation to be dropped
  private def convertLabel(in: String): String = {
    val m = LABEL_PAT.matcher(in)
    if (!m.matches()) {
      throw new IllegalArgumentException("Unable to convert label: " + in);
    }
    /* Black-Rook() -> black-rook */
    in.substring(0, in.length() - 2).toLowerCase();
  }
}