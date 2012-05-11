package chess.app

import java.util.regex.{ Matcher, Pattern }
import chess.library.Library
import chess.model.{ Colours, Move }
import chess.model.BoardModel
import chess.model.{ BoardChangedSubscriber, BoardChanged }
import chess.model.{ Castled, PiecePlaced, PieceMoved, PieceMovedTaking, Promoted, Resigned, Won }
import chess.model.Position
import chess.player.RandomPlayer
import chess.ui.UI
import chess.util.PlayerSelector
import chess.util.TODO
import test.AllTests
import chess.ui.{ Board, SwingBoard }

object ChessApp {
  def main(args: Array[String]) {

    runTests

    val ui = new UI
    val board = new BoardModel(BoardModel.standardPlacements, List(ui, BoardUI))

    /* The UI listens for changes and renders them immediately */
    ui.showBoard

    val white = new RandomPlayer(Colours.White, board.getConfiguration, board.getMoveExplorer)
    val black = new RandomPlayer(Colours.Black, board.getConfiguration, board.getMoveExplorer)
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

object BoardUI extends BoardChangedSubscriber {

  val board = SwingBoard.createAndShowBoard()

  def onBoardChanged(event: BoardChanged) {
    println("Board: " + event)

    def clearSquare(p: Position) = board.clearSquare(p.getCol, p.getRow)
    def setPiece(p: Position, piece: String) = board.setPiece(p.getCol, p.getRow, piece)
    def getPiece(p: Position) = board.getPiece(p.getCol, p.getRow)

    val DELAY_FACTOR = 1;

    event match {

      /* Assume the consumer of BoardChangeEvent has access to the board configuration. */
      case PiecePlaced(colour, piece, position) => {
        setPiece(position, convertLabel(colour + "-" + piece))
        Thread.sleep(DELAY_FACTOR * 2)
      }
      case PieceMoved(start, end) => {
        // TODO: Use a reference to a Configuration to acquire the piece from
        val label = getPiece(start)
        clearSquare(start)
        setPiece(end, label)
        Thread.sleep(DELAY_FACTOR * 100)
      }
      case PieceMovedTaking(start, end, taken) => {
        val label = getPiece(start)
        clearSquare(taken)
        clearSquare(start)
        setPiece(end, label)

        Thread.sleep(DELAY_FACTOR * 100)
      }
      case Promoted(position, piece) => {
        // TODO: Replace string handling with types
        val label = getPiece(position)
        /* white-pawn -> white-queen */
        val colour = label.substring(0, label.indexOf("-"))
        val promotedLabel = convertLabel(colour + "-" + piece)
        setPiece(position, promotedLabel)
        Thread.sleep(DELAY_FACTOR * 100)
      }
      case Castled(king, rook) => {
        val kingLabel = getPiece(king.start)
        val rookLabel = getPiece(rook.start)
        clearSquare(king.start)
        clearSquare(rook.start)
        setPiece(king.end, kingLabel)
        setPiece(rook.end, rookLabel)
      }
      case Resigned(colour) => {
        // TODO: Improve resignation visualisation with a popup dialog
        throw new RuntimeException(colour + " has resigned")
      }
      case Won(colour, winMode) => {
        board.showWon(colour.toString, winMode.toString)
      }
      // TODO: Add Won by check mate
      //case class Won(colour: Colour, winMode: WinMode) extends BoardChanged

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