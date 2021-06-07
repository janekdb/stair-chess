package chess.ranker

import chess.model.Colours.White
import chess.model.{Configuration, ConfigurationView, GridConfiguration, StandardMoveExplorer}
import chess.test.TestUtils
import org.scalatest._
import wordspec.AnyWordSpec
import matchers.should.Matchers

class ForkingRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A ForkingRanker" when {
    "given a choice of attacking move" should {
      "rank moves which attack multiple pieces first" in {
        forkingIsRankedFirst
      }
    }
  }

  private def forkingIsRankedFirst: Assertion = {
    val explorerFactory = (cv: ConfigurationView) => new StandardMoveExplorer(cv)
    val ranker = new ForkingRanker(explorerFactory, White)

    val conf: Configuration = new GridConfiguration

    addKings(conf)

//    add rook that can attack 1 or 2 pieces
//    add knight that can attack 1, 2 or 3 pieces
//    add tests
    fail()
  }
}
