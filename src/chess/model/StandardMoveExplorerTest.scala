package chess.model

import ex._

import test.{Test, TestUtils}

import Colours.{ Black, White }

object StandardMoveExplorerTest extends Test with TestUtils {

  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]) {
    runTests
  }

  def runTests() {

    getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveWhite
    getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveBlack
    getBasicPositionsIncludesEnPassantWhite
    getBasicPositionsIncludesEnPassantBlack
    getBasicPositionsExcludesEnPassantWhenNotDoubleAdvance
    getBasicPositionsExcludesEnPassantWhenNotAdjacentColumn
    getBasicPositionsExcludesEnPassantWhenNotFifthRow

    rejectIllegalMoveAllowsValidEnPassant
    rejectIllegalMoveRejectsNotAdjacentColumnEnPassant
    rejectIllegalMoveRejectsNotDoubleAdvanceEnPassant
    rejectIllegalMoveRejectsSelfCheckingEnPassant
    
    rejectIllegalMoveAllowsCastlingWhenRookOnlyCrossingAttackedSquare
    rejectIllegalMoveRejectsCastlingWhenKingCrossingAttackedSquare
  }

  private def getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveWhite {

    val start = new Position("e2")
    val end = new Position("e3")
    val conf = new GridConfiguration
    conf.add(start, White, Pawn())
    val e = new StandardMoveExplorer(conf)
    /* Move to remove the possibility of a two square advance. */
    conf.move(start, end)

    val actual = e.getBasicPositions(end)
    val expected: Set[Position] = Set("e4")
    assertEquals(expected, actual, "Position set excluded two square advance")
  }

  private def getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveBlack {

    val start = new Position("e7")
    val end = new Position("e6")
    val conf = new GridConfiguration
    conf.add(start, Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    /* Move to remove the possibility of a two square advance. */
    conf.move(start, end)

    val actual = e.getBasicPositions(end)
    val expected: Set[Position] = Set("e5")
    assertEquals(expected, actual, "Position set excluded two square advance")
  }

  
  /* All conditions met */
  private def getBasicPositionsIncludesEnPassantWhite {
    val whitePawnStart = new Position("e4")
    val whitePawnEnd = new Position("e5")
    val conf = new GridConfiguration
    conf.add(whitePawnStart, White, Pawn())
    conf.add("d7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    /* Move white to remove the possibility of a two square advance. */
    conf.move(whitePawnStart, whitePawnEnd)
    conf.move("d7", "d5")
    /*
     * The black pawn is now on the same rank as the white pawn and has moved two squares which is the
     * full set of pre-conditions required to allow white to capture via en-passant.
     */
    val actual = e.getBasicPositions(whitePawnEnd)
    val expected: Set[Position] = Set("d6", "e6")
    assertEquals(expected, actual, "Position set included en-passant")
  }


  /* All conditions met */
  private def getBasicPositionsIncludesEnPassantBlack {
    val blackPawnStart = new Position("e5")
    val blackPawnEnd = new Position("e4")
    val conf = new GridConfiguration
    conf.add(blackPawnStart, Black, Pawn())
    conf.add("d2", White, Pawn())
    val e = new StandardMoveExplorer(conf)
    /* Move black to remove the possibility of a two square advance. */
    conf.move(blackPawnStart, blackPawnEnd)
    conf.move("d2", "d4")
    /*
		   * The white pawn is now on the same rank as the black pawn and has moved two squares which is the
		   * full set of pre-conditions required to allow black to capture via en-passant.
		   */
    val actual = e.getBasicPositions(blackPawnEnd)
    val expected: Set[Position] = Set("d3", "e3")
    assertEquals(expected, actual, "Position set included en-passant")
  }

  /*
   * Check there is no en passant when one of the three necessary conditions is missing:
   * 1. Double advance, 2. Fifth rank, 3. Adjacent column.
   */

  /* No double advance */
  private def getBasicPositionsExcludesEnPassantWhenNotDoubleAdvance {
    val whitePawn = new Position("e2")
    val conf = new GridConfiguration
    conf.add(whitePawn, White, Pawn())
    conf.add("d3", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    /*
     * The captured pawn must have moved two squares immediately prior to the capture.
     */
    conf.move("d3", "d2")
    val actual = e.getBasicPositions(whitePawn)
    val expected: Set[Position] = Set("e4", "e3")
    assertEquals(expected, actual, "Position set excluded en-passant")
  }

  /* Not fifth row */
  private def getBasicPositionsExcludesEnPassantWhenNotFifthRow {
    val whitePawn = new Position("e2")
    val conf = new GridConfiguration
    conf.add(whitePawn, White, Pawn())
    conf.add("d7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    /*
     * The capturing pawn must be on the fifth row (white), fourth row (black).
     */
    conf.move("d7", "d5")
    val actual = e.getBasicPositions(whitePawn)
    val expected: Set[Position] = Set("e4", "e3")
    assertEquals(expected, actual, "Position set excluded en-passant")
  }

  /* Not adjacent column */
  private def getBasicPositionsExcludesEnPassantWhenNotAdjacentColumn {
    val whitePawnStart = new Position("e4")
    val whitePawnEnd = new Position("e5")
    val conf = new GridConfiguration
    conf.add(whitePawnStart, White, Pawn())
    conf.add("b7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    /* Move white to stop double advance move from being included. */
    conf.move(whitePawnStart, whitePawnEnd)
    /* The pieces must be on adjacent columns */
    conf.move("b7", "b5")
    val actual = e.getBasicPositions(whitePawnEnd)
    val expected: Set[Position] = Set("e6")
    assertEquals(expected, actual, "Position set excluded en-passant")
  }

  private def rejectIllegalMoveAllowsValidEnPassant {
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add("e5", White, Pawn())
    conf.add("f7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    conf.applyMove(MovePiece("f7", "f5"))
    e.rejectIllegalMove(EnPassant("e5", "f6"))
  }

  private def rejectIllegalMoveRejectsNotAdjacentColumnEnPassant {
    val conf = new GridConfiguration
    conf.add("e5", White, Pawn())
    conf.add("g7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    conf.move("g7", "g5")

    assertExceptionThrown("En-passant should be rejected when the adjacent column does not contain a pawn", classOf[UnreachablePositionException] ) {
      e.rejectIllegalMove(EnPassant("e5", "f6"))
    }
  }

  private def rejectIllegalMoveRejectsNotDoubleAdvanceEnPassant {
    val conf = new GridConfiguration
    conf.add("e4", White, Pawn())
    conf.add("d7", Black, Pawn())
    val e = new StandardMoveExplorer(conf)
    conf.move("d7", "d6")
    conf.move("e4", "e5")
    conf.move("d6", "d5")

    assertExceptionThrown("En-passant should be rejected when the previous move was not a double advance", classOf[UnreachablePositionException] ) {
      e.rejectIllegalMove(EnPassant("e5", "d6"))
    }
  }

  /* Do not overtest */
  //  private def rejectIllegalMoveRejectsIncorrectRowEnPassant = fail

  private def rejectIllegalMoveRejectsSelfCheckingEnPassant {
    val conf = new GridConfiguration
    conf.add("e5", White, Pawn())
    conf.add("d7", Black, Pawn())
    /* The rook that will check. */
    conf.add("e8", Black, Rook())
    conf.add("e1", White, King())
    val e = new StandardMoveExplorer(conf)
    conf.move("d7", "d5")
    
    assertExceptionThrown("En-passant should be rejected when the player would self check", classOf[CheckedOwnKing] ) {
      e.rejectIllegalMove(EnPassant("e5", "d6"))
    }
  }
  
  private def rejectIllegalMoveAllowsCastlingWhenRookOnlyCrossingAttackedSquare {
    val conf = new GridConfiguration
    /* The piece attacking a square the castling rook will cross. */
    conf.add("b2", White, Rook())
    /* The castling pieces */
    conf.add("e8", Black, King())
    conf.add("a8", Black, Rook())
    val e = new StandardMoveExplorer(conf)
    e.rejectIllegalMove(Castle(Black, Long))
  }

  private def rejectIllegalMoveRejectsCastlingWhenKingCrossingAttackedSquare {
    val conf = new GridConfiguration
    /* The piece attacking a square the castling king will cross. */
    conf.add("d2", White, Rook())
    /* The castling pieces */
    conf.add("e8", Black, King())
    conf.add("a8", Black, Rook())
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("Castling should be rejected when the king would cross an attacked square", classOf[AttackedPositionException]) {
      e.rejectIllegalMove(Castle(Black, Long))
    }
  }
  
  private def placeKings(conf: Configuration) {
    conf.add("e1", White, King())
    conf.add("e8", Black, King())
  }

}