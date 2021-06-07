package chess.ranker

import chess.model.Colours.{Black, White}
import chess.model._
import org.scalatest._
import wordspec.AnyWordSpec
import matchers.should.Matchers
import chess.test.TestUtils

//noinspection ZeroIndexToHead
class HighValueCapturingRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A HighValueCapturingRanker" when {
    "ranking" should {
      "rank high value capturing moves first" in {
        ranksHighValuePiecesFirst
      }
    }
  }

  private def ranksHighValuePiecesFirst: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker          = new HighValueCapturingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration
    addKings(conf)
    conf.add("a2", White, Rook)
    conf.add("b2", White, Rook)
    conf.add("c2", White, Rook)
    conf.add("a7", Black, Pawn)
    conf.add("b7", Black, Knight)
    conf.add("c7", Black, Queen)

    val explorer                      = new StandardMoveExplorer(conf)
    val rankedMoves: List[List[Move]] = ranker.rankMoves(explorer.legalMoves(White), conf)

    rankedMoves should have size 4
    rankedMoves(0) shouldBe new MovePieceCapturing("c2c7") :: Nil
    rankedMoves(1) shouldBe new MovePieceCapturing("b2b7") :: Nil
    rankedMoves(2) shouldBe new MovePieceCapturing("a2a7") :: Nil

    rankedMoves(3) should not be empty
  }
}
