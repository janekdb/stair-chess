package chess.model

import org.scalatest._
import wordspec.AnyWordSpec
import matchers.should.Matchers

class MoveTest extends AnyWordSpec with Matchers {

  private implicit class PositionHelper(val sc: StringContext) {
    def p(args: Any*): Position = new Position(sc.parts.head)
  }

  "A Move" should {
    "be rejected" when {
      "the start position equals the end position for a move" in {
        val p = p"e7"
        val caught = intercept[IllegalArgumentException] {
          MovePiece(p, p)
        }
        caught.getMessage should contain
        "different"
      }
      "the start position equals the end position for a capturing" in {
        val p = p"e7"
        val caught = intercept[IllegalArgumentException] {
          MovePieceCapturing(p, p)
        }
        caught.getMessage should contain
        "different"
      }
    }
    "identify the correct captured position" when {
      "en passant" in {
        // TODO: Check my understanding of en passant
        val e = EnPassant(p"e5", p"d6")
        e.captured shouldBe new Position("d5")
      }
      "identify the correct end position" when {
        "white moves" in {
          val wp = Promote(p"a7", Queen)
          wp.end shouldBe p"a8"
        }
        "black moves" in {
          val bp = Promote(p"a2", Queen)
          bp.end shouldBe p"a1"
        }
      }
    }
  }
}
