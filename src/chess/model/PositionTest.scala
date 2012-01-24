package chess.model

import test.Test

object PositionTest extends Test {

  def runTests(): Unit = {
    val i = Position.getInterveningPositions(new Position(1, 2), new Position(4, 2))
    println(List.range(2, 4))
    if (i != List(new Position(2, 2), new Position(3, 2))) {
      println(i)
      fail
    }
  }
}