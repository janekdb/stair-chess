package chess.ranker

import chess.model.Colours.{Black, White}
import chess.model._
import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec
import chess.test.TestUtils

class CapturingRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A CapturingRanker" when {
    "ranking" should {
      "rank capturing moves first" in {
        capturingIsRankedFirst
      }
    }
  }

  private def capturingIsRankedFirst: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker          = new CapturingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration

    addKings(conf)
    conf.add("h1", White, Rook)
    conf.add("f8", White, Bishop)
    conf.add("g8", White, Knight)
    conf.add("h8", Black, Rook)

    val explorer                      = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves should have size 2
    rankedMoves.head shouldBe new MovePieceCapturing("h1h8") :: Nil
  }

}
