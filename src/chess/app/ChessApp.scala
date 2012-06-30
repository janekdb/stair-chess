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
import chess.ui.TextUI
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
import scala.collection.mutable.HashMap

// ->TODO: Add a tournament mode
// TODO: Add an interactive mode
// TODO: Look for a way to be more functional
// TODO: Add player that moves pieces out of danger
// TODO: Drawn when only two Kings
// TODO: Drawn in other situations apart from two Kings where checkmate is not possible
object ChessApp {

  //  implicit def intWithTimes(n: Int): Unit = new {
  //    def times(f: => Unit) = 1 to n foreach { _ => f }
  //  }

  private def times(n: Int)(code: => Unit) {
    for (i <- 1 to n) code
  }

  def main(args: Array[String]) {

    runTests

    val scoreCard = new ScoreCard

    //    val white = new CheckingPlayer(Colours.White, board.getMoveExplorer)
    //    val black = new CheckingPlayer(Colours.Black, board.getMoveExplorer)
    //    val white = new CapturingPlayer(Colours.White, board.getMoveExplorer)
    //    val black = new RandomPlayer(Colours.Black, board.getMoveExplorer)
    //    val white = new DumbPlayer(Library.scholarsMate.whiteMoves)
    //    val black = new DumbPlayer(Library.scholarsMate.blackMoves)
    // TODO: For the tournament loop over all combinations of players
    val playerGenerator1 = ((colour: Colour, explorer: MoveExplorer) => new CheckingPlayer(colour, explorer))
    val playerGenerator2 = ((colour: Colour, explorer: MoveExplorer) => new CapturingPlayer(colour, explorer))
    val playerGenerator3 = ((colour: Colour, explorer: MoveExplorer) => new RandomPlayer(colour, explorer))
    val generators = playerGenerator1 :: playerGenerator2 :: playerGenerator3 :: List()
    times(1000) {
      for (wpg <- generators; bpg <- generators)
        try {
          play(scoreCard, wpg, bpg)
        } catch {
          // TODO: Remove this try/catch when the Castling error in defect-4.txt is fixed
          case e: Exception => println(e)
        }
    }
  }

  private val MAX_MOVES = 500

  private def play(scoreCard: ScoreCard, whitePlayerGenerator: (Colour, MoveExplorer) => Player, blackPlayerGenerator: (Colour, MoveExplorer) => Player) {
    val outcomeListener = new Object with BoardChangedSubscriber {
      var winner: Option[Colour] = None
      var isDrawn: Boolean = false
      def onBoardChanged(event: BoardChanged) {
        event match {
          case Won(colour, _) => winner = Some(colour)
          case Drawn(_) => isDrawn = true
          case default => Unit
        }
      }
    }

    val ui = new TextUI
    val boardAdapter = new BoardAdapter(SwingBoard.createAndShowBoard())
    val board = new BoardModel(BoardModel.standardPlacements, List(ui, boardAdapter, outcomeListener), List(boardAdapter))

    /* The UI listens for changes and renders them immediately */
    ui.showBoard

    val white = whitePlayerGenerator(Colours.White, board.getMoveExplorer)
    val black = blackPlayerGenerator(Colours.Black, board.getMoveExplorer)
    val playerSelector = new PlayerSelector(white, black)

    // TODO: Functionalise this maybe as a lazy seq from which the first MAX_MOVES are taken
    var moveCount = 0
    while (!board.isCompleted && moveCount < MAX_MOVES) {
      board.move(playerSelector.next.getMove(board.getConfiguration))
      moveCount += 1
    }

    val isAborted = moveCount == MAX_MOVES

    if (outcomeListener.winner.isDefined) {
      val colour = outcomeListener.winner.get
      val player = (if (colour == Colours.White) white else black).getClass().toString
      scoreCard.addWin(player)
    } else if (outcomeListener.isDrawn) {
      // TODO: Record the drawn
    } else if (isAborted) {
      // TODO: Record the abort as a draw
    } else {
      throw new AssertionError("Game completed in neither a win or draw")
    }

    /* Let the spectators note the final position. */
    delay

    boardAdapter.close
  }

  private def delay { TimeUnit.SECONDS.sleep(10) }

  def runTests {
    AllTests.runAllTests
  }
}

private class ScoreCard {
  val scores = new HashMap[String, Int]() { override def default(k: String) = 0 }

  def addWin(player: String) {
    println("Adding win for " + player)
    scores(player) = scores(player) + 1
    println("Scores: " + scores)
  }
}
