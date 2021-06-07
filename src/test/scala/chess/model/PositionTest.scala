package chess.model

import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec

class PositionTest extends AnyWordSpec with Matchers {

  "A Position" should {
    "calculate intervening positions correctly" in {
      val intervening = Position.getInterveningPositions(new Position(1, 2), new Position(4, 2))
      intervening shouldBe new Position(2, 2) :: new Position(3, 2) :: Nil
    }
  }
}
