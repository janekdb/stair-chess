package chess.model

import chess.model.Colours.Black
import chess.test.TestUtils
import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec

object DelegatingConfigurationViewTest extends AnyWordSpec with Matchers with TestUtils {

  "A DelegatingConfigurationView" should {
    "delegate" in {
      val conf = new GridConfiguration
      conf.add("e5", Black, Pawn)
      val del = new DelegatingConfigurationView(conf).applied(MovePiece("e5", "e6"))
      del.getExistingPiece("e6") shouldBe (Black, Pawn, Some(new Position("e5")))
    }
  }
}
