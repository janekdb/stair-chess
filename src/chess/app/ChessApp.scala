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
import chess.player.CheckingPlayer
import chess.player.Player
import chess.model.Colour
import chess.model.MoveExplorer
import chess.model.StandardMoveExplorer
import chess.ui.DelayingSubscriber
import chess.model.GameChangedSubscriber
import chess.model.GameChanged
import chess.model.Won
import chess.model.Drawn
import chess.player.Players
import chess.stage.ScoreCard
import chess.ui.NoUI

//Scores: Map(class chess.player.RandomPlayer -> 36, class chess.player.CapturingP
//layer -> 230, class chess.player.CheckingPlayer -> 103)

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
    val pg1 = ((colour: Colour, explorer: MoveExplorer) => new CheckingPlayer(colour, explorer, explorerFactory))
    //    val pg2 = ((colour: Colour, explorer: MoveExplorer) => Players.capturingPlayer(colour, explorerFactory))
    //    val pg3 = ((colour: Colour, explorer: MoveExplorer) => new RandomPlayer(colour, explorer))
    val pg3 = ((colour: Colour, explorer: MoveExplorer) => Players.checkMatingCapturingPlayer(colour, explorerFactory))
    val pg4 = ((colour: Colour, explorer: MoveExplorer) => Players.checkMatingCaptureEvadingPlayer(colour, explorerFactory))
    val generators = pg1 :: pg3 :: pg4 :: List()

    /* TODO: Stop duplicating player names be using a player category label that is not part of the player instance. */
    /* TODO: Add a trait for player generation to allow a getPlayerName method to be added */
    val scoreCard = new ScoreCard(Set("CheckingPlayer", "Checkmating, Capturing Player", "Checkmating, Capture Evading Player"))

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

    // TODO: Extract score card display into dedicated class
    println("Scores:")
    println("Wins:")

    val maxNameWidth = scoreCard.players.foldLeft(0) { (i, name) => i max name.length }
    val pad = (s: String) => { s.padTo(maxNameWidth, ' ') }
    val printScore = (name: String, score: Int) => println(pad(name) + " : " + "%4d" format score)

    scoreCard.getWins.foreach {
      case (name, score) => printScore(name, score)
    }
    println("Draws:")
    scoreCard.getDraws.foreach {
      case (name, score) => printScore(name, score)
    }

    /* Let the spectators note the final position. */
    delay(1)

    boardAdapter.foreach(_.close)
  }

  private def delay(count: Int = 1) { TimeUnit.SECONDS.sleep(count) }

  def runTests {
    AllTests.runAllTests
  }
}

