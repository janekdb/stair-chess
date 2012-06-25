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
import chess.ui.Board
import chess.ui.BoardAdapter
import chess.player.CapturingPlayer

// TODO: Add a player that prefers to check the opponents king
// TODO: Add a tournament mode
// TODO: Add an interactive mode
// TODO: Look for a way to be more functional
object ChessApp {
  def main(args: Array[String]) {

    runTests

    val ui = new UI
    val boardAdapter = new BoardAdapter(SwingBoard.createAndShowBoard())
    val board = new BoardModel(BoardModel.standardPlacements, List(ui, boardAdapter), List(boardAdapter))

    /* The UI listens for changes and renders them immediately */
    ui.showBoard

    val white = new CapturingPlayer(Colours.White, board.getMoveExplorer)
    val black = new RandomPlayer(Colours.Black, board.getMoveExplorer)
    //    val white = new DumbPlayer(Library.scholarsMate.whiteMoves)
    //    val black = new DumbPlayer(Library.scholarsMate.blackMoves)
    val playerSelector = new PlayerSelector(white, black)

    while (!board.isCompleted) {
      board.move(playerSelector.next.getMove(board.getConfiguration))
    }
  }

  def runTests {
    AllTests.runAllTests
  }
}
