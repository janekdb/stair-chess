package chess.model

import test.Test

object PositionTest extends Test {

  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests(): Unit = {
    val i = Position.getInterveningPositions(new Position(1, 2), new Position(4, 2))
    if (i != List(new Position(2, 2), new Position(3, 2))) {
      println(i)
      fail("The list of intervening positions was unexpected: " + i.toString)
    }
  }
}