package chess.player

import test.{ Main, Test, TestUtils }
import chess.model._
import chess.model.Colours._

object RandomPlayerTest extends Test with TestUtils with Main {

  implicit def optMove2Move(optMove: Option[Move]) = optMove.get

  def runTests {
    canMove
    isRandom
  }

  private def canMove {
    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    conf.add("a1", White, Rook())
    val rp = newRandomPlayer(conf)
    assertNotNull(rp.getMove(conf.copyOf), "A move should be available")
  }

  private def isRandom {
    val conf: Configuration = new GridConfiguration
    addWhiteKing(conf)
    conf.add("a1", White, Rook())
    val rp = newRandomPlayer(conf)
    val m1 = rp.getMove(conf.copyOf)
    conf.applyMove(m1)
    val m2 = rp.getMove(conf.copyOf)
    assertNotEquals(m1, m2, "The second move should be different to the first")
  }

  private def newRandomPlayer(conf: Configuration): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new RandomPlayer(White, explorer)
  }

  private def newRandomPlayer(conf: Configuration, colour: Colour): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new RandomPlayer(colour, explorer)
  }
}