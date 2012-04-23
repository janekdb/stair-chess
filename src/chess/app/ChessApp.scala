package chess.app

import chess.model.{ Colours, Move }
import chess.model.BoardModel
import chess.model.{BoardChangedSubscriber, BoardChanged}
import chess.model.{Castled, PiecePlaced, PieceMoved, PieceMovedTaking, Promoted, Resigned}
import chess.model.Position
import chess.player.{ Computer, Human }
import chess.ui.UI
import chess.util.PlayerSelector
import chess.util.TODO
import test.AllTests
import chess.ui.{Board, SwingBoard}

object ChessApp {
  def main(args: Array[String]) {

    runTests

    val ui = new UI
    val board = new BoardModel(BoardModel.standardPlacements, List(ui, BoardUI))

    /* The UI listens for changes and renders them immediately */
    ui.showBoard

    val white = new Computer(board, Colours.White)
    val black = new Human(board, Colours.Black)
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
    // TODO: Use label in this method
    def label(p: Position): String = board.getLabel(p.getCol, p.getRow)
    def clearLabel(p: Position) = board.setLabel(p.getCol, p.getRow, "")
    def setLabel(p: Position, label: String) = board.setLabel(p.getCol, p.getRow, label)
    event match {

      /* Assume the consumer of BoardChangeEvent has access to the board configuration. */
      case PiecePlaced(colour, piece, position) => {
        setLabel(position, colour + ": " + piece)
        Thread.sleep(10)
      }
      case PieceMoved(start, end) => {
        // TODO: Use a reference to a Configuration to acquire the piece from
        val label = board.getLabel(start.getCol, start.getRow)
        clearLabel(start)
        setLabel(end, label)
        Thread.sleep(100)
      }
      case PieceMovedTaking(start, end, taken) => {
        val label = board.getLabel(start.getCol, start.getRow)
        clearLabel(taken)
        clearLabel(start)
        setLabel(end, label)

        Thread.sleep(100)
      }
      case Promoted(position, piece) => {
        val label = board.getLabel(position.getCol, position.getRow)
        setLabel(position, piece + "->" + label)
        Thread.sleep(100)
      }
      case Castled(king, rook) => {
        val kingLabel = label(king.start)
        val rookLabel = label(rook.start)
        clearLabel(king.start)
        clearLabel(rook.start)
        setLabel(king.end, kingLabel)
        setLabel(rook.end, rookLabel)
      }
      case Resigned(colour) => {
        // TODO: Improve resignation visualisation with a popup dialog
        board.setLabel(1, 1, colour + " resigns!")
      }
      // TODO: Add Won by check mate
      //case class Won(colour: Colour, winMode: WinMode) extends BoardChanged

      case default => TODO.throwRuntimeEx("Unhandled case: " + event)
    }
  }

}