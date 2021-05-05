package chess.ranker

import chess.model.Colours.White
import chess.model._
import chess.test.TestUtils
import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec

//noinspection ZeroIndexToHead
class CheckingRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A CheckingRanker" when {
    "ranking" should {
      "rank checking moves first" in {
        alwaysChecks
      }
    }
  }

  private def alwaysChecks: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new CheckingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration
    addKings(conf)
    conf.add("c4", White, Knight)
    conf.add("g5", White, Pawn)
    val expectedKnightMove = new MovePiece("c4d6")

    val explorer = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    /* Visual inspection of the ranked moves confirmed the partition count. */
    rankedMoves should have size 2
    rankedMoves(0) shouldBe expectedKnightMove :: Nil
    rankedMoves(1) should contain(new MovePiece("g5g6"))
  }
}
