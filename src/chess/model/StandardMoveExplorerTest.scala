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
    getBasicPositionsExcludesTwoSquarePawnAdvanceWhenFirstMoveButNotOnStartPosition
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
    val actual = e.getBasicPositions(whitePawn)
    val expected: Set[Position] = Set("e4", "e3")
    assert(expected == actual, "Incorrect position set: expected: " + expected + ", actual: " + actual)
  }

  private def getBasicPositionsIncludesEnPassantOnFifthRank = {
    val whitePawnStart = new Position("e4")
    val whitePawnEnd = new Position("e5")
    val conf = new GridConfiguration
    conf.add(whitePawnStart, White, Pawn())
    conf.add("d7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    /*
     * The black pawn is now on the same rank as the white pawn and has moved two squares which is the
     * full set of pre-conditions required to allow white to capture via en-passant.
     */
    /* Move white to remove the possibility of a two square advance. */
    conf.move(whitePawnStart, whitePawnEnd)
    conf.move("d7", "d5")
    val actual = e.getBasicPositions(whitePawnEnd)
    val expected: Set[Position] = Set("d6", "e6")
    assert(expected == actual, "Incorrect position set: expected: " + expected + ", actual: " + actual)
  }
  
  private def getBasicPositionsExcludesTwoSquarePawnAdvanceWhenFirstMoveButNotOnStartPosition = fail
}