package chess.model

import ex._

import test.{ Main, Test, TestUtils }

import Colours.{ Black, White }

object StandardMoveExplorerTest extends Test with TestUtils with Main {

  def runTests {

    acceptMovePieceThatWouldNotCapture
    acceptPromoteThatWouldNotCapture

    rejectMovePieceThatWouldCapture
    rejectPromoteThatWouldCapture

    acceptMovePieceCapturingThatWouldCapture
    acceptPromoteCapturingThatWouldCapture

    rejectMovePieceCapturingThatWouldNotCapture
    rejectPromoteCapturingThatWouldNotCapture

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
    rejectIllegalMoveRejectsCastlingWhenKingCrossingSquareAttackedByPawn
    rejectIllegalMoveRejectsReCastling

    rejectIllegalMoveAllowsResigning

    rejectNonPromotingPawnAdvanceToBackRank
    rejectNonPromotingPawnCaptureOfBackRankPiece

    /* legalMoves */
    selectsOnlyMove
    pawnPromotionSelected
    pawnCapturingPromotionSelected
    queenCaptureSelected
    queenCaptureSelected2
    castlingIncluded
    shortCastlingNotConsideredWhenStartPositionsIncorrect
    longCastlingNotConsideredWhenStartPositionsIncorrect
    castlingNotConsideredWhenEitherPieceIsNotOwnPiece
    castlingNotConsiderWhenOutsidePieceIsNotRook
    castlingNotConsiderWhenInsidePieceIsNotKing
    enPassantSelected

    /* Check and Check Mate */
    kingIsCheckMatedDetected

    /* Defects */
    defect5IsFixed
  }

  private def acceptMovePieceThatWouldNotCapture {
    val start = new Position("e2")
    val end = new Position("e3")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn())
    val e = new StandardMoveExplorer(conf)
    e.rejectIllegalMove(MovePiece(start, end))
  }

  private def acceptPromoteThatWouldNotCapture {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn())
    val e = new StandardMoveExplorer(conf)
    e.rejectIllegalMove(Promote(start, Queen()))
  }

  private def rejectMovePieceThatWouldCapture {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Rook())
    conf.add(end, Black, Rook())
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("MovePiece that would have taken a piece was rejected", classOf[NonCapturingMoveException]) {
      e.rejectIllegalMove(MovePiece(start, end))
    }
  }

  private def rejectPromoteThatWouldCapture {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn())
    conf.add(end, Black, Rook())
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("Promote that would have taken a piece was rejected", classOf[UnreachablePositionException]) {
      e.rejectIllegalMove(Promote(start, Queen()))
    }
  }

  private def acceptMovePieceCapturingThatWouldCapture {
    val start = new Position("a7")
    val end = new Position("b8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Bishop())
    conf.add(end, Black, Rook())
    val e = new StandardMoveExplorer(conf)
    e.rejectIllegalMove(MovePieceCapturing(start, end))
  }

  private def acceptPromoteCapturingThatWouldCapture {
    val start = new Position("a7")
    val end = new Position("b8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn())
    conf.add(end, Black, Rook())
    val e = new StandardMoveExplorer(conf)
    e.rejectIllegalMove(PromoteCapturing(start, end, Rook()))
  }

  private def rejectMovePieceCapturingThatWouldNotCapture {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Rook())
    conf.add("f5", Black, Rook())
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("MovePieceCapturing that would not have captured a piece was rejected", classOf[CapturingMoveException]) {
      e.rejectIllegalMove(MovePieceCapturing(start, end))
    }
  }

  private def rejectPromoteCapturingThatWouldNotCapture {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn())
    conf.add("f5", Black, Rook())
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("PromoteCapturing that would not have capturing a piece was rejected", classOf[CapturingMoveException]) {
      e.rejectIllegalMove(PromoteCapturing(start, end, Queen()))
    }
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

    assertExceptionThrown("En-passant should be rejected when the adjacent column does not contain a pawn", classOf[UnreachablePositionException]) {
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

    assertExceptionThrown("En-passant should be rejected when the previous move was not a double advance", classOf[UnreachablePositionException]) {
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

    assertExceptionThrown("En-passant should be rejected when the player would self check", classOf[CheckedOwnKing]) {
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

  /* Confirm the fault exhibited by defect 6 is fixed. */
  private def rejectIllegalMoveRejectsCastlingWhenKingCrossingSquareAttackedByPawn {
    val conf = new GridConfiguration
    placeAll(conf)
    for (move <- DefectFixture.defect6Moves) {
      conf.applyMove(move)
    }
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("Castling should be rejected when the king would end on a square attacked by a pawn", classOf[AttackedPositionException]) {
      val move = DefectFixture.defect6FinalMove
      e.rejectIllegalMove(move)
      render(conf)
    }
  }

  private def rejectIllegalMoveRejectsReCastling {
    val conf = new GridConfiguration
    val moveExplorer = new StandardMoveExplorer(conf)

    conf.add("a1", White, Rook())
    conf.add("e1", White, King())
    conf.add("e8", Black, King())

    /* Castle */
    conf.applyMove(Castle(White, Long))
    /* Move King and Rook back to castling start position */
    conf.applyMove("e8e7")
    conf.applyMove("d1d2")
    conf.applyMove("e7e8")
    conf.applyMove("d2a2")
    conf.applyMove("e8e7")
    conf.applyMove("a2a1")
    conf.applyMove("c1d1")
    conf.applyMove("e7e8")
    conf.applyMove("d1e1")
    conf.applyMove("e8e7")

    assertExceptionThrown("Re-Castling was rejected", classOf[PreviouslyMovedException]) {
      moveExplorer.rejectIllegalMove(Castle(White, Long))
    }
  }

  private def rejectIllegalMoveAllowsResigning {
    val conf = new GridConfiguration
    val moveExplorer = new StandardMoveExplorer(conf)

    conf.add("e1", White, King())
    conf.add("e8", Black, King())

    moveExplorer.rejectIllegalMove(Resign(White))
  }

  private def rejectNonPromotingPawnAdvanceToBackRank {
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add("d7", White, Pawn())
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("Non promoting pawn advance to opponents home rank should be rejected", classOf[NonPromotingPawnAdvance]) {
      e.rejectIllegalMove(MovePiece("d7", "d8"))
    }
  }

  private def rejectNonPromotingPawnCaptureOfBackRankPiece {
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add("d7", White, Pawn())
    conf.add("e8", Black, Bishop())
    val e = new StandardMoveExplorer(conf)
    assertExceptionThrown("Non promoting pawn capturing of opponents home rank piece should be rejected", classOf[NonPromotingPawnAdvance]) {
      e.rejectIllegalMove(MovePieceCapturing("d7", "e8"))
    }
  }

  /* legalMoves: start */
  private def selectsOnlyMove {
    val conf: Configuration = new GridConfiguration
    /* Box the rooks in */
    // Rr    PK
    // RP    PP
    // P
    conf.add("a6", White, Pawn());
    conf.add("a7", White, Rook());
    conf.add("b7", White, Pawn());
    conf.add("a8", White, Rook());
    conf.add("b8", Black, Rook());
    /* Box the White king in */
    conf.add("h8", White, King());
    conf.add("h7", White, Pawn());
    conf.add("g8", White, Pawn());
    conf.add("g7", White, Pawn());

    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White)
    assertEquals(List(MovePieceCapturing("a8", "b8")), moves, "The only possible move should have been selected")
  }

  private def pawnPromotionSelected {
    val conf: Configuration = new GridConfiguration
    /* The pawn that should be promoted */
    conf.add("b7", White, Pawn())
    conf.add("h8", White, King());

    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: Promote => true case default => false }
    val promote = Promote("b7", Queen())
    val expected = List(promote.copy(piece = Knight()), promote)
    assertEquals(expected, moves, "Pawn promotion to both Queen and Knight was considered")
  }

  private def pawnCapturingPromotionSelected {
    val conf: Configuration = new GridConfiguration
    placeKings(conf)
    /* The pawn that should be promoted */
    conf.add("b7", White, Pawn())
    conf.add("c8", Black, Rook())

    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: PromoteCapturing => true case default => false }
    val promote = PromoteCapturing("b7", "c8", Queen())
    val expected = List(promote.copy(piece = Knight()), promote)
    assertEquals(expected, moves, "Capturing pawn promotion to both Queen and Knight was considered")
  }

  //  abcdefgh
  //8 ········
  //7 ········
  //6 ······q·
  //5 ········
  //4 ·K······
  //3 ···kQ···
  //2 ·······Q
  //1 ··.b····
  //  abcdefgh
  /* Black can escape checkmate by taking the queen */
  private def queenCaptureSelected {
    val conf: Configuration = new GridConfiguration
    conf.add("d1", Black, Bishop())
    conf.add("h2", White, Queen())
    conf.add("d3", Black, King())
    conf.add("e3", White, Queen())
    conf.add("b4", White, King())
    conf.add("g6", Black, Queen())
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(Black)
    assertEquals(List(MovePieceCapturing("d3", "e3")), moves, "Black escaped from check by selected the only possible move")
  }

  //  abcdefgh
  //8 ······k·
  //7 ·····Q··
  //6 ········
  //5 ········
  //4 ···B····
  //3 pR·p·p··
  //2 ·····P··
  //1 ······K·
  //  abcdefgh
  /* Black can escape checkmate by taking the queen */
  private def queenCaptureSelected2 {
    val conf: Configuration = new GridConfiguration
    conf.add("g1", White, King())
    conf.add("f2", Black, Pawn())
    conf.add("a3", Black, Pawn())
    conf.add("b3", White, Rook())
    conf.add("d3", White, Pawn())
    conf.add("f3", White, Pawn())
    conf.add("d4", White, Bishop())
    conf.add("f7", White, Queen())
    conf.add("g8", Black, King())
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(Black)
    assertEquals(List(MovePieceCapturing("g8", "f7")), moves, "Black escaped from check by selected the only possible move")
  }

  private def castlingIncluded {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook())
    conf.add("e1", White, King())
    conf.add("h1", White, Rook())
    val allowedMove = Castle(White, Long)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: Castle => true case default => false }
    assertEquals(List(Castle(White, Short), Castle(White, Long)), moves, "Both long and short castling was included")
  }

  private def shortCastlingNotConsideredWhenStartPositionsIncorrect {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook())
    conf.add("e1", White, King())
    conf.add("h2", White, Rook())
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: Castle => true case default => false }
    assertEquals(List(Castle(White, Long)), moves, "When only long castling was possible short castling was not considered")
  }

  private def longCastlingNotConsideredWhenStartPositionsIncorrect {
    val conf: Configuration = new GridConfiguration
    conf.add("a2", White, Rook())
    conf.add("e1", White, King())
    conf.add("h1", White, Rook())
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: Castle => true case default => false }
    assertEquals(List(Castle(White, Short)), moves, "When only short castling was possible long castling was not considered")
  }

  private def castlingNotConsideredWhenEitherPieceIsNotOwnPiece {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", Black, Rook())
    conf.add("e1", White, King())
    conf.add("h1", Black, Rook())
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: Castle => true case default => false }
    assertEquals(List(), moves, "When no castling was possible no castling was considered")
  }

  private def castlingNotConsiderWhenOutsidePieceIsNotRook {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Knight())
    conf.add("e1", White, King())
    conf.add("h1", White, Bishop())
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: Castle => true case default => false }
    assertEquals(List(), moves, "When the outside piece was not a rook no castling was considered")
  }

  private def castlingNotConsiderWhenInsidePieceIsNotKing {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook())
    conf.add("d1", White, King())
    conf.add("e1", White, Queen())
    conf.add("h1", White, Rook())
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case a: Castle => true case default => false }
    assertEquals(List(), moves, "When the inside piece was not a king  no castling was considered")
  }

  private def enPassantSelected {
    val conf: Configuration = new GridConfiguration
    placeKings(conf)
    conf.add("e7", Black, Pawn())
    conf.add("d4", White, Pawn())
    /* Do not allow double advance */
    conf.applyMove(MovePiece("d4", "d5"))
    /* Allow pawn to be captured with en passant */
    conf.applyMove(MovePiece("e7", "e5"))
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White)
    assertFalse(moves contains MovePiece("d5", "e6"), "MovePiece should not have been in the list of acceptable moves")
    assertTrue(moves contains EnPassant("d5", "e6"), "En passant was in the list of acceptable moves")
  }

  /* legalMoves: end*/

  /* Check Mate start */

  private def kingIsCheckMatedDetected {
    val conf: Configuration = new GridConfiguration
    placeKings(conf)

    val e1 = new StandardMoveExplorer(conf)
    assertFalse(e1.kingInCheck(Black), "The black king was not in check")
    assertFalse(e1.kingInCheckMate(Black), "The black king was not checkmated")

    conf.add("a8", White, Rook())
    val e2 = new StandardMoveExplorer(conf)
    assertTrue(e2.kingInCheck(Black), "The black king was in check")
    assertFalse(e2.kingInCheckMate(Black), "The black king was not checkmated")

    conf.add("a7", White, Rook())
    val e3 = new StandardMoveExplorer(conf)
    assertTrue(e3.kingInCheck(Black), "The black king was in check")
    assertTrue(e3.kingInCheckMate(Black), "The black king was checkmated")
  }

  /* Check Mate end */

  /* defect: start */

  private def defect5IsFixed {
    val conf: Configuration = new GridConfiguration
    placeAll(conf)
    for (move <- DefectFixture.defect5Moves) {
      conf.applyMove(move)
    }
    val e = new StandardMoveExplorer(conf)
    val move = DefectFixture.defect5FinalMove
    assertExceptionThrown(move + " should be rejected", classOf[InvalidParticipantException]) {
      e.rejectIllegalMove(move)
    }
  }

  /* defect: end */

  private def placeKings(conf: Configuration) {
    conf.add("e1", White, King())
    conf.add("e8", Black, King())
  }

  private def placeAll(conf: Configuration) {
    for ((colour, piece, position) <- BoardModel.standardPlacements)
      conf.add(position, colour, piece)
  }

  private def render(conf: ConfigurationView) {
    val lines = ConfigurationView.getTextRepresentation(conf)
    for (line <- lines) println(line)
    println
  }

}