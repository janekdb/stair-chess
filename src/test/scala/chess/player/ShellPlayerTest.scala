package chess.player

import chess.model.Colours.White
import chess.model._
import chess.ranker.MoveRanker
import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec
import OptionValues._
import test.TestUtils

// TODO: Mixin a piece value source to influence capturing player
// TODO: Modify MoveRanker to require each move to be scored rather than partitioned
class ShellPlayerTest extends AnyWordSpec with Matchers with TestUtils {

  "A ShellPlayer" when {
    "providing a move" should {
      "apply the move ranker" in {
        usesGroupingFunction()
      }
    }
  }

  private def usesGroupingFunction(): Assertion = {

    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    conf.add("a1", White, Rook)
    conf.add("b1", White, Knight)
    conf.add("c1", White, Bishop)
    val mr = new Object with MoveRanker {
      def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
        val knightMoves = moves filter {
          case move: SimpleMove =>
            val (_, piece, _) = conf.getExistingPiece(move.start)
            piece == Knight
          case _ => false
        }
        assert(knightMoves.nonEmpty)
        val otherMoves = moves filterNot (knightMoves.contains)
        List(knightMoves, otherMoves)
      }
    }
    val sp = newShellPlayer(mr)

    val move = sp.getMove(conf.copyOf)
    move.value shouldBe a[MovePiece]
    val start = move.value match {
      case mp: MovePiece => mp.start
      case _ => fail()
    }

    val (colour, piece, _) = conf.getExistingPiece(start)
    colour shouldBe White
    piece shouldBe Knight
  }

  private def newShellPlayer(moveRanker: MoveRanker): Player = {
    val explorerFactory = (conf: ConfigurationView) => new StandardMoveExplorer(conf)
    new ShellPlayer("Test", White, explorerFactory, moveRanker)
  }
}
