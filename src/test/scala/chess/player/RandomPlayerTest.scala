package chess.player

import chess.model.Colour.White
import chess.model.*
import org.scalatest.*
import matchers.should.Matchers
import wordspec.AnyWordSpec
import chess.test.TestUtils

class RandomPlayerTest extends AnyWordSpec with Matchers with TestUtils {

  given Conversion[Option[Move], Move] = _.get

  "A RandomPlayer" when {
    "playing" should {
      "play a move" in {
        canMove()
      }
      "play moves randomly" in {
        isRandom()
      }
    }
  }

  private def canMove(): Assertion = {
    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    conf.add("a1", White, Rook)
    val rp = newRandomPlayer(conf)
    rp.getMove(conf.copyOf) shouldBe defined
  }

  private def isRandom(): Assertion = {
    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    conf.add("a1", White, Rook)
    val rp = newRandomPlayer(conf)
    val m1 = rp.getMove(conf.copyOf)
    conf.applyMove(m1)
    val m2 = rp.getMove(conf.copyOf)
    m1 should not be m2
  }

  private def newRandomPlayer(conf: Configuration): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new RandomPlayer(White, explorer)
  }
}
