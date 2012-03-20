package chess.app

import chess.model.{ Colours, Move }
import chess.model.BoardModel
import chess.player.{ Computer, Human }
import chess.ui.UI
import chess.util.PlayerSelector
import chess.util.TODO
import test.AllTests

object ChessApp {
  def main(args: Array[String]): Unit = {

    runTests

    val ui = new UI
    val board = new BoardModel(BoardModel.standardPlacements, List(ui))

    /* The UI listens for changes and renders them immediately */
    ui.showBoard

    val white = new Computer(board, Colours.White)
    val black = new Human(board, Colours.Black)
    val playerSelector = new PlayerSelector(white, black)

    while (!board.isWon) {
      board.move(playerSelector.next.getMove)
    }
    TODO.throwRuntimeEx("Find out why the board is empty since the use of the BoardModel placements constructor")
  }

  def runTests: Unit = {
    AllTests.runAllTests
  }
}
