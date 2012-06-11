package chess.app

import java.util.concurrent.TimeUnit
import java.util.regex.Pattern
import chess.model.BoardChanged
import chess.model.BoardChangedSubscriber
import chess.model.BoardModel
import chess.model.Castled
import chess.model.Colours
import chess.model.PieceMoved
import chess.model.PieceMovedCapturing
import chess.model.PiecePlaced
import chess.model.Position
import chess.model.Promoted
import chess.model.Resigned
import chess.model.Won
import chess.player.RandomPlayer
import chess.ui.SwingBoard
import chess.ui.UI
import chess.util.PlayerSelector
import chess.util.TODO
import test.AllTests
import chess.model.ConfigurationChangedSubscriber
import chess.model.Configuration
import chess.model.ConfigurationView

object ChessApp {
  def main(args: Array[String]) {

    runTests

    val ui = new UI
    val board = new BoardModel(BoardModel.standardPlacements, List(ui, BoardUI), List(BoardUI))

    /* The UI listens for changes and renders them immediately */
    ui.showBoard

    val white = new RandomPlayer(Colours.White, board.getMoveExplorer)
    val black = new RandomPlayer(Colours.Black, board.getMoveExplorer)
    //    val white = new DumbPlayer(Library.scholarsMate.whiteMoves)
    //    val black = new DumbPlayer(Library.scholarsMate.blackMoves)
    val playerSelector = new PlayerSelector(white, black)

    while (!board.isWon) {
      board.move(playerSelector.next.getMove)
    }
  }

  def runTests {
    AllTests.runAllTests
  }
}

// TODO: Move BoardUI into a class
object BoardUI extends BoardChangedSubscriber with ConfigurationChangedSubscriber {

  val board = SwingBoard.createAndShowBoard()

  var configuration: ConfigurationView = null

  def onConfigurationChanged(event: ConfigurationView) {
    this.configuration = event
  }

  def onBoardChanged(event: BoardChanged) {
    println("Board: " + event)

    def clearSquare(p: Position) = board.clearSquare(p.getCol, p.getRow)
    def setPiece(p: Position, piece: String) = board.setPiece(p.getCol, p.getRow, piece)
    // TODO: Remove getPiece from board trait
//    def getPiece(p: Position) = board.getPiece(p.getCol, p.getRow)

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
        // TODO: Improve resignation visualisation with a popup dialog
        throw new RuntimeException(colour + " has resigned")
      }
      case Won(colour, winMode) => {
        board.showWon(colour.toString, winMode.toString)
      }

      case default => TODO.throwRuntimeEx("Unhandled case: " + event)
    }
  }

  val LABEL_PAT = Pattern.compile("(black|white)++-[a-z]++\\(\\)", Pattern.CASE_INSENSITIVE);

  private def convertLabel(in: String): String = {
    val m = LABEL_PAT.matcher(in)
    if (!m.matches()) {
      throw new IllegalArgumentException("Unable to convert label: " + in);
    }
    /* Black-Rook() -> black-rook */
    in.substring(0, in.length() - 2).toLowerCase();
  }
}