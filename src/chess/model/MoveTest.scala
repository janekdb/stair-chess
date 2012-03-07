package chess.model

import test.Test

object MoveTest extends Test {

  // TODO: Define these test helping implicits in a common location
  implicit def piece2List(t: Piece) = List(t)
  implicit def string2Position(s: String) = new Position(s)
  //  implicit def string2MovePiece(s: String) = new MovePiece(s)

  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests(): Unit = {
    enPassant
  }

  private def enPassant = {
    val e = EnPassant("e5", "d6")
    assertEquals(new Position("d5"), e.taken)
  }

}