package chess.model

import test.{Main, Test}

object PositionTest extends Test with Main {

  def runTests() {
    val i = Position.getInterveningPositions(new Position(1, 2), new Position(4, 2))
    if (i != List(new Position(2, 2), new Position(3, 2))) {
      println(i)
      fail("The list of intervening positions was unexpected: " + i.toString)
    }
  }
}