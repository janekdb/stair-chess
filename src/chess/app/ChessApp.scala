package chess.app

import java.util.concurrent.TimeUnit

import chess.model.BoardModel
import chess.model.Colour
import chess.model.Colours
import chess.model.Configuration
import chess.model.ConfigurationView
import chess.model.Drawn
import chess.model.GameChanged
import chess.model.GameChangedSubscriber
import chess.model.MoveExplorer
import chess.model.StandardMoveExplorer
import chess.model.Won
import chess.player.Player
import chess.player.Players
import chess.stage.Display
import chess.stage.ScoreCard
import chess.ui.Board
import chess.ui.BoardAdapter
import chess.ui.DelayingSubscriber
import chess.ui.NoUI
import chess.ui.SwingBoard
import chess.ui.TextUI
import chess.util.PlayerSelector
import test.AllTests

// TODO: Add a tournament mode
// TODO: Score as tournament
// TODO: Add an interactive mode
// TODO: Look for a way to be more functional
// TODO: Force draw when only two Kings
// TODO: Drawn in other situations apart from two Kings where checkmate is not possible
// TODO: Add stalemate avoidance
// TODO: Add a developed position preferring player
// TODO: Add lookahead ranker with configurable depth
// TODO: Add parrallelism to lookahead ranker
// TODO: UI: Convert SwingBoard to Scala

object ChessApp {

  //  implicit def intWithTimes(n: Int): Unit = new {
  //    def times(f: => Unit) = 1 to n foreach { _ => f }
  //  }

  private def times(n: Int)(code: => Unit) {
    for (i <- 1 to n) code
  }

  def main(args: Array[String]) {

    runTests

    // TODO: For the tournament loop over all combinations of players
    val explorerFactory = (conf: ConfigurationView) => new StandardMoveExplorer(conf)

    val checkingName = "Checking Player"
    val checkMatingCapturingName = "Checkmating, Capturing Player"
    val checkMatingCaptureEvadingName = "Checkmating, Capture Evading Player"

    val p1 = (checkingName, (colour: Colour, explorer: MoveExplorer) => Players.checkingPlayer(checkingName, colour, explorerFactory))
    val p2 = (checkMatingCapturingName, (colour: Colour, explorer: MoveExplorer) => Players.checkMatingCapturingPlayer(checkMatingCapturingName, colour, explorerFactory))
    val p3 = (checkMatingCaptureEvadingName, (colour: Colour, explorer: MoveExplorer) => Players.checkMatingCaptureEvadingPlayer(checkMatingCaptureEvadingName, colour, explorerFactory))
    val ps = List(p1, p2, p3)

    val names = ps map { _._1 }
    val scoreCard = new ScoreCard(names toSet)

    val generators = ps map { _._2 }

    times(1000) {
      for (wpg <- generators; bpg <- generators; if wpg != bpg)
        play(scoreCard, wpg, bpg)
    }
  }

  private val MAX_MOVES = 200

  private def play(scoreCard: ScoreCard, whitePlayerGenerator: (Colour, MoveExplorer) => Player, blackPlayerGenerator: (Colour, MoveExplorer) => Player) {
    val outcomeListener = new Object with GameChangedSubscriber {
      var winner: Option[Colour] = None
      var isDrawn: Boolean = false
      def onGameChanged(event: GameChanged) {
        event match {
          case Won(colour, _) => winner = Some(colour)
          case Drawn(_) => isDrawn = true
          case default => Unit
        }
      }
    }

    val useSwingBoard = false
    val useTextUI = false
    val includeDelay = false
    val boardAdapter = if (useSwingBoard)
      // TODO: Remove visual side-effect from SwingBoard creation
      Some(new BoardAdapter(SwingBoard.createAndShowBoard()))
    else
      None

    val ui = if (useTextUI) new TextUI else NoUI

    val delayer = if (includeDelay) new DelayingSubscriber else NoUI

    val boardChangedSubscribers = boardAdapter.toList ++ List(ui, delayer)
    val board = new BoardModel(BoardModel.standardPlacements, boardChangedSubscribers,
      boardAdapter.toList, List(ui, outcomeListener, delayer))

    val white = whitePlayerGenerator(Colours.White, board.getMoveExplorer)
    val black = blackPlayerGenerator(Colours.Black, board.getMoveExplorer)
    val playerSelector = new PlayerSelector(white, black)

    //    if(false ){
    //
    //    var moveCount = 0
    //    while (!board.isCompleted && moveCount < MAX_MOVES) {
    //      board.move(playerSelector.next.getMove(board.getConfiguration))
    //      moveCount += 1
    //    }
    //    }

    // TODO: Continue functionalising this maybe as a lazy seq from which the first MAX_MOVES are taken
    val moveCount = (
      for {
        m <- 1 to MAX_MOVES
        if !board.isCompleted
      } yield {
        board.move(playerSelector.next.getMove(board.getConfiguration))
        m
      }).max

    val isAborted = moveCount == MAX_MOVES

    if (outcomeListener.winner.isDefined) {
      val colour = outcomeListener.winner.get
      val (winner, loser) = if (colour == Colours.White) (white, black) else (black, white)
      scoreCard.addWin(winner, loser)
    } else if (outcomeListener.isDrawn) {
      scoreCard.addDraw(white, black)
    } else if (isAborted) {
      scoreCard.addDraw(white, black)
    } else {
      throw new AssertionError("Game completed in neither a win or draw")
    }

    Display.renderScoreCard(scoreCard)

    /* Let the spectators note the final position. */
    delay(1)

    boardAdapter.foreach(_.close)
  }

  private def delay(count: Int = 1) { TimeUnit.SECONDS.sleep(count) }

  def runTests {
    AllTests.runAllTests
  }
}

