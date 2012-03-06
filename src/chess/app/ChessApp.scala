package chess.app

import chess.model.{ Colours, Move }
import chess.model.BoardModel
import chess.player.{ Computer, Human }
import chess.ui.UI
import chess.util.PlayerSelector

object ChessApp {
  def main(args: Array[String]): Unit = {

    runTests

    val board = new BoardModel
    /* The UI listens for changes and renders them immediately */
    new UI(board).showBoard
    board.placePieces

    val white = new Computer(board, Colours.White)
    val black = new Human(board, Colours.Black)
    val playerSelector = new PlayerSelector(white, black)

    while (!board.isWon) {
      board.move(playerSelector.next.getMove)
    }
  }

  def runTests: Unit = {
    // TODO: Determine if each test object could add itself into a object
    //   which would allow all tests to be looped over to run them.
    chess.model.PositionTest.runTests
    chess.model.MoveTest.runTests
    chess.model.GridConfigurationTest.runTests
    chess.model.BoardModelTest.runTests
    chess.model.StandardMoveExplorerTest.runTests
  }
}
