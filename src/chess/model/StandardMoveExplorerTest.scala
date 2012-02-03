package chess.model

import test.Test

import Colours.{ Black, White }

object StandardMoveExplorerTest extends Test {

  // TODO: Define this test helping implicits in a common location
  implicit def piece2List(t: Piece) = List(t)
  implicit def string2Position(s: String) = new Position(s)
  implicit def string2MovePiece(s: String) = new MovePiece(s)

  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests(): Unit = {

    // TODO: Add test that a white pawn on e3 does not include e5 in the list of basic positions
    getBasicPositionsExcludesEnPassantOnNonFifthRank
    getBasicPositionsIncludesEnPassantOnFifthRank
  }

  private def getBasicPositionsExcludesEnPassantOnNonFifthRank = {
    val whitePawn = new Position("e2")
    val conf = new GridConfiguration
    conf.add(whitePawn, White, Pawn())
    conf.add("d3", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    /*
     * The black pawn is now on the same rank as the white pawn which a pre-condition for en passant
     * but not a sufficient condition. The captured pawn must have moved two squares immediately prior
     * to the capture.
     */
    conf.move("d3", "d2")
    val ps = e.getBasicPositions(whitePawn)
    // TODO: Find out how to convert a List to a Set so the order of the positions is not tested.
    val expected: List[Position] = List("e4", "e3")
    var actual: List[Position] = ps
    assert(expected == actual, "Incorrect list of positions: expected: " + expected + ", actual: " + actual)
  }

  private def getBasicPositionsIncludesEnPassantOnFifthRank = {
    // TODO: Write this test
    val whitePawnStart = new Position("e4")
    val whitePawnEnd = new Position("e5")
    val conf = new GridConfiguration
    conf.add(whitePawnStart, White, Pawn())
    conf.add("d7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    // TODO: Update this comment
    /*
     * The black pawn is now on the same rank as the white pawn which a pre-condition for en passant
     * but not a sufficient condition. The captured pawn must have moved two squares immediately prior
     * to the capture.
     */
    conf.move(whitePawnStart, whitePawnEnd)
    conf.move("d7", "d5")
    val ps = e.getBasicPositions(whitePawnEnd)
    // TODO: Find out how to convert a List to a Set so the order of the positions is not tested.
    val expected: List[Position] = List("d6", "e6")
    var actual: List[Position] = ps
    assert(expected == actual, "Incorrect list of positions: expected: " + expected + ", actual: " + actual)
    fail
  }
}