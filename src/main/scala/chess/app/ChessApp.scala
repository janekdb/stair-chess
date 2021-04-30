package chess.app

import java.util.concurrent.TimeUnit
import chess.model.BoardModel
import chess.model.Colour
import chess.model.Colours.{Black, White}
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
import chess.ui.MoveEntryListener
import chess.player.Human
import chess.player.BlockingPlayer
import chess.model.MovePiece
import chess.model.StandardMoveParser
import chess.model.MoveFactory
import chess.model.Move
import scala.language.postfixOps

// TODO: ->Add an interactive mode
// TODO:   ->Disable text entry when not user's turn
// TODO:   Do not ask for a move from the player when no moves are possible
// TODO: Add a tournament mode
// TODO: Score as tournament
// TODO: Look for a way to be more functional
// TODO: Force draw when only two Kings
// TODO: Drawn in other situations apart from two Kings where checkmate is not possible
// TODO: Add stalemate avoidance
// TODO: Add a developed position preferring player
// TODO: Add lookahead ranker with configurable depth
// TODO: Add parallelism to lookahead ranker
// TODO: UI: Convert SwingBoard to Scala
// TODO: Prevent a pawn at h2 from taking a queen at h4 as happened in a manual interaction

object ChessApp {

  //  implicit def intWithTimes(n: Int): Unit = new {
  //    def times(f: => Unit) = 1 to n foreach { _ => f }
  //  }

  private def times(n: Int)(code: => Unit): Unit = {
    for (i <- 1 to n) code
  }

  def main(args: Array[String]): Unit = {

    runTests

    // TODO: For the tournament loop over all combinations of players
    val explorerFactory = (conf: ConfigurationView) => new StandardMoveExplorer(conf)

    val checkingName = "Checking Player"
    val checkMatingCapturingName = "CM, Cap Player"
    val checkMatingCaptureEvadingName = "CM, CapEva Player"
    val checkMatingCaptureEvadingCapturingName = "CM, CapEva, Cap Player"
    val checkMatingHighValueCapturingName = "CM, HV Cap Player"

    val p1 = (checkingName, (colour: Colour, explorer: MoveExplorer) => Players.checkingPlayer(checkingName, colour, explorerFactory))
    val p2 = (checkMatingCapturingName, (colour: Colour, explorer: MoveExplorer) => Players.checkMatingCapturingPlayer(checkMatingCapturingName, colour, explorerFactory))
    val p3 = (checkMatingCaptureEvadingName, (colour: Colour, explorer: MoveExplorer) => Players.checkMatingCaptureEvadingPlayer(checkMatingCaptureEvadingName, colour, explorerFactory))
    val p4 = (checkMatingCaptureEvadingCapturingName, (colour: Colour, explorer: MoveExplorer) => Players.checkMatingCaptureEvadingCheckingPlayer(checkMatingCaptureEvadingCapturingName, colour, explorerFactory))
    val p5 = (checkMatingHighValueCapturingName, (colour: Colour, explorer: MoveExplorer) => Players.checkMatingHighValueCapturingPlayer(checkMatingHighValueCapturingName, colour, explorerFactory))

    val interactive = false
    if (interactive) {
      val playerName = "Human"
      // TODO: Stop assuming BlockingPlayer will be Black
      val player = new BlockingPlayer(Black, playerName)
      val blockingPlayerGenerator = (playerName, (colour: Colour, explorer: MoveExplorer) => player)
      val ps = List(p1, blockingPlayerGenerator)
      val names = ps map (_._1)
      val scoreCard = new ScoreCard(names toSet)

      val generators = ps map (_._2)

      val interactiveMode = true
      val sb = SwingBoard.createAndShowBoard(interactiveMode)

      /* Invoked when a legal move has been parsed. */
      val moveListener = new MoveListener {
        def onMove(move: Move): Unit = {
          println("MoveListener: " + move)
          sb.clearMoveEntry()
        }
      }
      val playerListener: MoveEntryListener = mel(player, moveListener)
      sb.addMoveEntryListener(playerListener)
      val boardAdapterOpt = Some(new BoardAdapter(sb))
      // TODO: Stop using a tuple for blockingPlayerGenerator
      play(scoreCard, boardAdapterOpt, generators.head, blockingPlayerGenerator._2)
    } else {
      val ps = List(p1, p2, p3, p4, p5)
      val names = ps map (_._1)
      val scoreCard = new ScoreCard(names toSet)

      val generators = ps map (_._2)
      times(1000) {
        for (wpg <- generators; bpg <- generators; if wpg != bpg) {
          val useSwingBoard = true
          val boardAdapterOpt = if (useSwingBoard) {
            // TODO: Remove visual side-effect from SwingBoard creation
            val interactiveMode = false
            Some(new BoardAdapter(SwingBoard.createAndShowBoard(interactiveMode)))
          } else
            None

          play(scoreCard, boardAdapterOpt, wpg, bpg)
        }
      }
    }
  }

  private trait MoveListener {
    def onMove(move: Move): Unit
  }

  private def mel(player: BlockingPlayer, legalMoveListener: MoveListener) = {
    class MF(val text: String) extends MoveFactory {
      def getMove(colour: Colour, conf: Configuration): Option[Move] = {
        val e = new StandardMoveExplorer(conf)
        val moves = e.legalMoves(player.getColour)
        //    Console.out.println("legalMoves: " + moves)
        val moveOpt = StandardMoveParser.parse(moves, text)
        if (moveOpt.isDefined) {
          // TODO: Remove println from MoveFactory implementation
          println("Sending event to legalMoveListener: " + moveOpt)
          legalMoveListener.onMove(moveOpt.get)
        }
        moveOpt
      }
    }
    new MoveEntryListener {
      def onMoveEntry(text: String): Unit = {
        player.setMoveFactory(new MF(text))
      }
    }
  }

  private val MaxMoves = 200

  private class OutcomeListener extends GameChangedSubscriber {
    var winner: Option[Colour] = None
    var isDrawn: Boolean = false

    def onGameChanged(event: GameChanged): Unit = {
      event match {
        case Won(colour, _) => winner = Some(colour)
        case Drawn(_) => isDrawn = true
      }
    }
  }

  private def play(scoreCard: ScoreCard, boardAdapterOpt: Option[BoardAdapter], whitePlayerGenerator: (Colour, MoveExplorer) => Player, blackPlayerGenerator: (Colour, MoveExplorer) => Player): Unit = {
    val outcomeListener = new OutcomeListener

    val useTextUI = false
    val includeDelay = false

    val ui = if (useTextUI) new TextUI else NoUI

    val delayer = if (includeDelay) new DelayingSubscriber else NoUI

    val boardChangedSubscribers = boardAdapterOpt.toList ++ List(ui, delayer)
    val board = new BoardModel(BoardModel.standardPlacements, boardChangedSubscribers,
      boardAdapterOpt.toList, List(ui, outcomeListener, delayer))

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
        m <- 1 to MaxMoves
        if !board.isCompleted
      } yield {
        board.move(playerSelector.next().getMove(board.getConfiguration))
        m
      }).max

    val isAborted = moveCount == MaxMoves

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

    boardAdapterOpt.foreach(_.close)
  }

  private def delay(count: Int = 1): Unit = {
    TimeUnit.SECONDS.sleep(count)
  }

  def runTests: Unit = {
    AllTests.runAllTests
  }
}

