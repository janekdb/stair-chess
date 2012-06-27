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
import chess.player.CheckingPlayer
import chess.player.Player
import chess.model.Colour
import chess.model.MoveExplorer
import chess.model.Drawn

// ->TODO: Add a tournament mode
// TODO: Add an interactive mode
// TODO: Look for a way to be more functional
// TODO: Add player that moves pieces out of danger
object ChessApp {
  def main(args: Array[String]) {

    runTests

    //    val white = new CheckingPlayer(Colours.White, board.getMoveExplorer)
    //    val black = new CheckingPlayer(Colours.Black, board.getMoveExplorer)
    //    val white = new CapturingPlayer(Colours.White, board.getMoveExplorer)
    //    val black = new RandomPlayer(Colours.Black, board.getMoveExplorer)
    //    val white = new DumbPlayer(Library.scholarsMate.whiteMoves)
    //    val black = new DumbPlayer(Library.scholarsMate.blackMoves)
    // TODO: For the tournament loop over all combinations of players
    val playerGenerator = ((colour: Colour, explorer: MoveExplorer) => new CheckingPlayer(colour, explorer))
    play(playerGenerator)
  }

  private def play(playerGenerator: (Colour, MoveExplorer) => Player) {
    val outcomeListener = new Object with BoardChangedSubscriber {
      var winner: Option[Colour] = None
      var isDrawn: Boolean = false
      def onBoardChanged(event: BoardChanged) {
        println("outcomeListener: " + event)
        event match {
          case Won(colour, _) => winner = Some(colour)
          case Drawn(_) => isDrawn = true
          case default => Unit
        }
      }
    }

    val ui = new UI
    val boardAdapter = new BoardAdapter(SwingBoard.createAndShowBoard())
    val board = new BoardModel(BoardModel.standardPlacements, List(ui, boardAdapter, outcomeListener), List(boardAdapter))

    /* The UI listens for changes and renders them immediately */
    ui.showBoard

    val white = playerGenerator(Colours.White, board.getMoveExplorer)
    val black = playerGenerator(Colours.Black, board.getMoveExplorer)
    val playerSelector = new PlayerSelector(white, black)

    while (!board.isCompleted) {
      board.move(playerSelector.next.getMove(board.getConfiguration))
    }

    if (outcomeListener.winner.isDefined) {
      // TODO: Record the win
    } else if (outcomeListener.isDrawn) {
      // TODO: Record the drawn
    } else {
      throw new AssertionError("Game completed in neither a win or draw")
    }
    // TODO: Close Board after completion or 400 moves
  }

  def runTests {
    AllTests.runAllTests
  }
}

