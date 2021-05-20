package chess.model

import chess.model.Colours.{Black, White}
import chess.model.ex._
import chess.test.TestUtils
import org.scalatest._
import wordspec.AnyWordSpec
import matchers.should.Matchers
import LoneElement._

class StandardMoveExplorerTest extends AnyWordSpec with Matchers with TestUtils {

  "A StandardMoveExplorer" should {
    "accept and reject moves" in {
      acceptMovePieceThatWouldNotCapture()
      acceptPromoteThatWouldNotCapture()

      rejectMovePieceThatWouldCapture()
      rejectPromoteThatWouldCapture()

      acceptMovePieceCapturingThatWouldCapture()
      acceptPromoteCapturingThatWouldCapture()

      rejectMovePieceCapturingThatWouldNotCapture()
      rejectPromoteCapturingThatWouldNotCapture()

      getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveWhite()
      getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveBlack()
      getBasicPositionsIncludesEnPassantWhite()
      getBasicPositionsIncludesEnPassantBlack()
      getBasicPositionsExcludesEnPassantWhenNotDoubleAdvance()
      getBasicPositionsExcludesEnPassantWhenNotAdjacentColumn()
      getBasicPositionsExcludesEnPassantWhenNotFifthRow()
      rejectIllegalMoveAllowsValidEnPassant()
      rejectIllegalMoveRejectsNotAdjacentColumnEnPassant()
      rejectIllegalMoveRejectsNotDoubleAdvanceEnPassant()
      rejectIllegalMoveRejectsSelfCheckingEnPassant()

      rejectIllegalMoveAllowsCastlingWhenRookOnlyCrossingAttackedSquare()
      rejectIllegalMoveRejectsCastlingWhenKingCrossingAttackedSquare()
      rejectIllegalMoveRejectsCastlingWhenKingCrossingSquareAttackedByPawn()
      rejectIllegalMoveRejectsReCastling()

      //    rejectIllegalMoveAllowsResigning

      rejectNonPromotingPawnAdvanceToBackRank()
      rejectNonPromotingPawnCaptureOfBackRankPiece()

      /* legalMoves */
      selectsOnlyMove()
      pawnPromotionSelected()
      pawnCapturingPromotionSelected()
      queenCaptureSelected()
      queenCaptureSelected2()
      castlingIncluded()
      shortCastlingNotConsideredWhenStartPositionsIncorrect()
      longCastlingNotConsideredWhenStartPositionsIncorrect()
      castlingNotConsideredWhenEitherPieceIsNotOwnPiece()
      castlingNotConsiderWhenOutsidePieceIsNotRook()
      castlingNotConsiderWhenInsidePieceIsNotKing()
      enPassantSelected()
      resignExcluded()

      /* Check and Check Mate */
      kingIsCheckMatedDetected()

      /* Defects */
      defect5IsFixed()
    }
  }

  private def placeKings(conf: Configuration): Unit = {
    conf.add("e1", White, King)
    conf.add("e8", Black, King)
  }

  private def placeAll(conf: Configuration): Unit = {
    for ((colour, piece, position) <- BoardModel.standardPlacements)
      conf.add(position, colour, piece)
  }

  private def render(conf: ConfigurationView): Unit = {
    val lines = ConfigurationView.getTextRepresentation(conf)
    for (line <- lines) println(line)
    println()
  }

  private def acceptMovePieceThatWouldNotCapture(): Assertion = {
    val start = new Position("e2")
    val end = new Position("e3")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn)
    val e = new StandardMoveExplorer(conf)
    noException shouldBe thrownBy(e.rejectIllegalMove(MovePiece(start, end)))
  }

  private def acceptPromoteThatWouldNotCapture(): Assertion = {
    val start = new Position("a7")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn)
    val e = new StandardMoveExplorer(conf)
    noException shouldBe thrownBy(e.rejectIllegalMove(Promote(start, Queen)))
  }

  private def rejectMovePieceThatWouldCapture(): Assertion = {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Rook)
    conf.add(end, Black, Rook)
    val e = new StandardMoveExplorer(conf)
    withClue("MovePiece that would have taken a piece was rejected") {
      a[NonCapturingMoveException] shouldBe thrownBy {
        e.rejectIllegalMove(MovePiece(start, end))
      }
    }
  }

  private def rejectPromoteThatWouldCapture(): Assertion = {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn)
    conf.add(end, Black, Rook)
    val e = new StandardMoveExplorer(conf)
    withClue("Promote that would have taken a piece was rejected") {
      a[UnreachablePositionException] shouldBe thrownBy {
        e.rejectIllegalMove(Promote(start, Queen))
      }
    }
  }

  private def acceptMovePieceCapturingThatWouldCapture(): Assertion = {
    val start = new Position("a7")
    val end = new Position("b8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Bishop)
    conf.add(end, Black, Rook)
    val e = new StandardMoveExplorer(conf)
    noException shouldBe thrownBy(e.rejectIllegalMove(MovePieceCapturing(start, end)))
  }

  private def acceptPromoteCapturingThatWouldCapture(): Assertion = {
    val start = new Position("a7")
    val end = new Position("b8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn)
    conf.add(end, Black, Rook)
    val e = new StandardMoveExplorer(conf)
    noException shouldBe thrownBy(e.rejectIllegalMove(PromoteCapturing(start, end, Rook)))
  }

  private def rejectMovePieceCapturingThatWouldNotCapture(): Assertion = {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Rook)
    conf.add("f5", Black, Rook)
    val e = new StandardMoveExplorer(conf)
    withClue("MovePieceCapturing that would not have captured a piece was rejected") {
      a[CapturingMoveException] shouldBe thrownBy {
        e.rejectIllegalMove(MovePieceCapturing(start, end))
      }
    }
  }

  private def rejectPromoteCapturingThatWouldNotCapture(): Assertion = {
    val start = new Position("a7")
    val end = new Position("a8")
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add(start, White, Pawn)
    conf.add("f5", Black, Rook)
    val e = new StandardMoveExplorer(conf)
    withClue("PromoteCapturing that would not have capturing a piece was rejected") {
      a[CapturingMoveException] shouldBe thrownBy {
        e.rejectIllegalMove(PromoteCapturing(start, end, Queen))
      }
    }
  }

  private def getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveWhite(): Assertion = {

    val start = new Position("e2")
    val end = new Position("e3")
    val conf = new GridConfiguration
    conf.add(start, White, Pawn)
    val e = new StandardMoveExplorer(conf)
    /* Move to remove the possibility of a two square advance. */
    conf.move(start, end)

    val actual = e.getBasicPositions(end)
    val expected: Set[Position] = Set("e4")
    withClue("Position set excluded two square advance") {
      actual shouldBe expected
    }
  }

  private def getBasicPositionsExcludesDoubleAdvanceWhenNotFirstMoveBlack(): Assertion = {

    val start = new Position("e7")
    val end = new Position("e6")
    val conf = new GridConfiguration
    conf.add(start, Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    /* Move to remove the possibility of a two square advance. */
    conf.move(start, end)

    val actual = e.getBasicPositions(end)
    val expected: Set[Position] = Set("e5")
    withClue("Position set excluded two square advance") {
      actual shouldBe expected
    }
  }

  /* All conditions met */
  private def getBasicPositionsIncludesEnPassantWhite(): Assertion = {
    val whitePawnStart = new Position("e4")
    val whitePawnEnd = new Position("e5")
    val conf = new GridConfiguration
    conf.add(whitePawnStart, White, Pawn)
    conf.add("d7", Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    /* Move white to remove the possibility of a two square advance. */
    conf.move(whitePawnStart, whitePawnEnd)
    conf.move("d7", "d5")
    /*
     * The black pawn is now on the same rank as the white pawn and has moved two squares which is the
     * full set of pre-conditions required to allow white to capture via en passant.
     */
    val actual = e.getBasicPositions(whitePawnEnd)
    val expected: Set[Position] = Set("d6", "e6")
    withClue("Position set included en passant") {
      actual shouldBe expected
    }
  }

  /* All conditions met */
  private def getBasicPositionsIncludesEnPassantBlack(): Assertion = {
    val blackPawnStart = new Position("e5")
    val blackPawnEnd = new Position("e4")
    val conf = new GridConfiguration
    conf.add(blackPawnStart, Black, Pawn)
    conf.add("d2", White, Pawn)
    val e = new StandardMoveExplorer(conf)
    /* Move black to remove the possibility of a two square advance. */
    conf.move(blackPawnStart, blackPawnEnd)
    conf.move("d2", "d4")
    /*
		   * The white pawn is now on the same rank as the black pawn and has moved two squares which is the
		   * full set of pre-conditions required to allow black to capture via en passant.
		   */
    val actual = e.getBasicPositions(blackPawnEnd)
    val expected: Set[Position] = Set("d3", "e3")
    withClue("Position set included en passant") {
      actual shouldBe expected
    }
  }

  /*
   * Check there is no en passant when one of the three necessary conditions is missing:
   * 1. Double advance, 2. Fifth rank, 3. Adjacent column.
   */

  /* No double advance */
  private def getBasicPositionsExcludesEnPassantWhenNotDoubleAdvance(): Assertion = {
    val whitePawn = new Position("e2")
    val conf = new GridConfiguration
    conf.add(whitePawn, White, Pawn)
    conf.add("d3", Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    /*
     * The captured pawn must have moved two squares immediately prior to the capture.
     */
    conf.move("d3", "d2")
    val actual = e.getBasicPositions(whitePawn)
    val expected: Set[Position] = Set("e4", "e3")
    withClue("Position set excluded en passant") {
      actual shouldBe expected
    }
  }

  /* Not fifth row */
  private def getBasicPositionsExcludesEnPassantWhenNotFifthRow(): Assertion = {
    val whitePawn = new Position("e2")
    val conf = new GridConfiguration
    conf.add(whitePawn, White, Pawn)
    conf.add("d7", Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    /*
     * The capturing pawn must be on the fifth row (white), fourth row (black).
     */
    conf.move("d7", "d5")
    val actual = e.getBasicPositions(whitePawn)
    val expected: Set[Position] = Set("e4", "e3")
    withClue("Position set excluded en passant") {
      actual shouldBe expected
    }
  }

  /* Not adjacent column */
  private def getBasicPositionsExcludesEnPassantWhenNotAdjacentColumn(): Assertion = {
    val whitePawnStart = new Position("e4")
    val whitePawnEnd = new Position("e5")
    val conf = new GridConfiguration
    conf.add(whitePawnStart, White, Pawn)
    conf.add("b7", Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    /* Move white to stop double advance move from being included. */
    conf.move(whitePawnStart, whitePawnEnd)
    /* The pieces must be on adjacent columns */
    conf.move("b7", "b5")
    val actual = e.getBasicPositions(whitePawnEnd)
    val expected: Set[Position] = Set("e6")
    withClue("Position set excluded en passant") {
      actual shouldBe expected
    }
  }

  private def rejectIllegalMoveAllowsValidEnPassant(): Assertion = {
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add("e5", White, Pawn)
    conf.add("f7", Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    conf.applyMove(MovePiece("f7", "f5"))
    noException shouldBe thrownBy(e.rejectIllegalMove(EnPassant("e5", "f6")))
  }

  private def rejectIllegalMoveRejectsNotAdjacentColumnEnPassant(): Assertion = {
    val conf = new GridConfiguration
    conf.add("e5", White, Pawn)
    conf.add("g7", Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    conf.move("g7", "g5")
    withClue("En passant should be rejected when the adjacent column does not contain a pawn") {
      an[UnreachablePositionException] shouldBe thrownBy {
        e.rejectIllegalMove(EnPassant("e5", "f6"))
      }

    }
  }

  private def rejectIllegalMoveRejectsNotDoubleAdvanceEnPassant(): Assertion = {
    val conf = new GridConfiguration
    conf.add("e4", White, Pawn)
    conf.add("d7", Black, Pawn)
    val e = new StandardMoveExplorer(conf)
    conf.move("d7", "d6")
    conf.move("e4", "e5")
    conf.move("d6", "d5")

    withClue("En passant should be rejected when the previous move was not a double advance") {
      an[UnreachablePositionException] shouldBe thrownBy {
        e.rejectIllegalMove(EnPassant("e5", "d6"))
      }
    }
  }

  /* Do not overtest */
  //  private def rejectIllegalMoveRejectsIncorrectRowEnPassant = fail

  private def rejectIllegalMoveRejectsSelfCheckingEnPassant(): Assertion = {
    val conf = new GridConfiguration
    conf.add("e5", White, Pawn)
    conf.add("d7", Black, Pawn)
    /* The rook that will check. */
    conf.add("e8", Black, Rook)
    conf.add("e1", White, King)
    val e = new StandardMoveExplorer(conf)
    conf.move("d7", "d5")

    withClue("En passant should be rejected when the player would self check") {
      a[CheckedOwnKing] shouldBe thrownBy {
        e.rejectIllegalMove(EnPassant("e5", "d6"))
      }
    }
  }

  private def rejectIllegalMoveAllowsCastlingWhenRookOnlyCrossingAttackedSquare(): Assertion = {
    val conf = new GridConfiguration
    /* The piece attacking a square the castling rook will cross. */
    conf.add("b2", White, Rook)
    /* The castling pieces */
    conf.add("e8", Black, King)
    conf.add("a8", Black, Rook)
    val e = new StandardMoveExplorer(conf)
    noException shouldBe thrownBy(e.rejectIllegalMove(Castle(Black, Long)))
  }

  private def rejectIllegalMoveRejectsCastlingWhenKingCrossingAttackedSquare(): Assertion = {
    val conf = new GridConfiguration
    /* The piece attacking a square the castling king will cross. */
    conf.add("d2", White, Rook)
    /* The castling pieces */
    conf.add("e8", Black, King)
    conf.add("a8", Black, Rook)
    val e = new StandardMoveExplorer(conf)
    withClue("Castling should be rejected when the king would cross an attacked square") {
      an[AttackedPositionException] shouldBe thrownBy {
        e.rejectIllegalMove(Castle(Black, Long))
      }
    }
  }

  /* Confirm the fault exhibited by defect 6 is fixed. */
  private def rejectIllegalMoveRejectsCastlingWhenKingCrossingSquareAttackedByPawn(): Assertion = {
    val conf = new GridConfiguration
    placeAll(conf)
    for (move <- DefectFixture.defect6Moves) {
      conf.applyMove(move)
    }
    val e = new StandardMoveExplorer(conf)
    withClue("Castling should be rejected when the king would end on a square attacked by a pawn") {
      val move = DefectFixture.defect6FinalMove
      an[AttackedPositionException] shouldBe thrownBy {
        e.rejectIllegalMove(move)
      }
    }
  }

  private def rejectIllegalMoveRejectsReCastling(): Assertion = {
    val conf = new GridConfiguration
    val moveExplorer = new StandardMoveExplorer(conf)

    conf.add("a1", White, Rook)
    conf.add("e1", White, King)
    conf.add("e8", Black, King)

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

    withClue("Re-Castling was rejected") {
      a[PreviouslyMovedException] shouldBe thrownBy {
        moveExplorer.rejectIllegalMove(Castle(White, Long))
      }
    }
  }

  private def rejectNonPromotingPawnAdvanceToBackRank(): Assertion = {
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add("d7", White, Pawn)
    val e = new StandardMoveExplorer(conf)
    withClue("Non promoting pawn advance to opponents home rank should be rejected") {
      a[NonPromotingPawnAdvance] shouldBe thrownBy(e.rejectIllegalMove(MovePiece("d7", "d8")))
    }
  }

  private def rejectNonPromotingPawnCaptureOfBackRankPiece(): Assertion = {
    val conf = new GridConfiguration
    placeKings(conf)
    conf.add("d7", White, Pawn)
    conf.add("e8", Black, Bishop)
    val e = new StandardMoveExplorer(conf)
    withClue("Non promoting pawn capturing of opponents home rank piece should be rejected") {
      a[NonPromotingPawnAdvance] shouldBe thrownBy(e.rejectIllegalMove(MovePieceCapturing("d7", "e8")))
    }
  }

  /* legalMoves: start */
  private def selectsOnlyMove(): Assertion = {
    val conf: Configuration = new GridConfiguration
    /* Box the rooks in */
    // Rr    PK
    // RP    PP
    // P
    conf.add("a6", White, Pawn)
    conf.add("a7", White, Rook)
    conf.add("b7", White, Pawn)
    conf.add("a8", White, Rook)
    conf.add("b8", Black, Rook)
    /* Box the White king in */
    conf.add("h8", White, King)
    conf.add("h7", White, Pawn)
    conf.add("g8", White, Pawn)
    conf.add("g7", White, Pawn)

    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White)
    withClue("The only possible move should have been selected") {
      moves.loneElement shouldBe MovePieceCapturing("a8", "b8")
    }
  }

  private def pawnPromotionSelected(): Assertion = {
    val conf: Configuration = new GridConfiguration
    /* The pawn that should be promoted */
    conf.add("b7", White, Pawn)
    conf.add("h8", White, King)

    val e = new StandardMoveExplorer(conf)
    // TODO: Use collect
    val moves = e.legalMoves(White) filter { case _: Promote => true case _ => false }
    val promote = Promote("b7", Queen)
    val expected = List(promote.copy(piece = Knight), promote)
    withClue("Pawn promotion to both Queen and Knight was considered") {
      moves shouldBe expected
    }
  }

  private def pawnCapturingPromotionSelected(): Assertion = {
    val conf: Configuration = new GridConfiguration
    placeKings(conf)
    /* The pawn that should be promoted */
    conf.add("b7", White, Pawn)
    conf.add("c8", Black, Rook)

    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case _: PromoteCapturing => true case _ => false }
    val promote = PromoteCapturing("b7", "c8", Queen)
    val expected = List(promote.copy(piece = Knight), promote)
    withClue("Capturing pawn promotion to both Queen and Knight was considered") {
      moves shouldBe expected
    }
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
  private def queenCaptureSelected(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("d1", Black, Bishop)
    conf.add("h2", White, Queen)
    conf.add("d3", Black, King)
    conf.add("e3", White, Queen)
    conf.add("b4", White, King)
    conf.add("g6", Black, Queen)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(Black)
    withClue("Black escaped from check by selecting the only possible move") {
      moves.loneElement shouldBe MovePieceCapturing("d3", "e3")
    }
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
  private def queenCaptureSelected2(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("g1", White, King)
    conf.add("f2", Black, Pawn)
    conf.add("a3", Black, Pawn)
    conf.add("b3", White, Rook)
    conf.add("d3", White, Pawn)
    conf.add("f3", White, Pawn)
    conf.add("d4", White, Bishop)
    conf.add("f7", White, Queen)
    conf.add("g8", Black, King)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(Black)
    withClue("Black escaped from check by selected the only possible move") {
      moves.loneElement shouldBe MovePieceCapturing("g8", "f7")
    }
  }

  private def castlingIncluded(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook)
    conf.add("e1", White, King)
    conf.add("h1", White, Rook)
    val e = new StandardMoveExplorer(conf)
    // TODO: Use collect
    val moves = e.legalMoves(White) filter { case _: Castle => true case _ => false }
    withClue("Both long and short castling was included") {
      moves shouldBe Castle(White, Short) :: Castle(White, Long) :: Nil
    }

  }

  private def shortCastlingNotConsideredWhenStartPositionsIncorrect(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook)
    conf.add("e1", White, King)
    conf.add("h2", White, Rook)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case _: Castle => true case _ => false }
    withClue("When only long castling was possible short castling was not considered") {
      moves.loneElement shouldBe Castle(White, Long)
    }
  }

  private def longCastlingNotConsideredWhenStartPositionsIncorrect(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("a2", White, Rook)
    conf.add("e1", White, King)
    conf.add("h1", White, Rook)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case _: Castle => true case _ => false }
    withClue("When only short castling was possible long castling was not considered") {
      moves.loneElement shouldBe Castle(White, Short)
    }
  }

  private def castlingNotConsideredWhenEitherPieceIsNotOwnPiece(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", Black, Rook)
    conf.add("e1", White, King)
    conf.add("h1", Black, Rook)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case _: Castle => true case _ => false }
    withClue("When no castling was possible no castling was considered") {
      moves shouldBe empty
    }
  }

  private def castlingNotConsiderWhenOutsidePieceIsNotRook(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Knight)
    conf.add("e1", White, King)
    conf.add("h1", White, Bishop)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case _: Castle => true case _ => false }
    withClue("When the outside piece was not a rook no castling was considered") {
      moves shouldBe empty
    }
  }

  private def castlingNotConsiderWhenInsidePieceIsNotKing(): Assertion = {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook)
    conf.add("d1", White, King)
    conf.add("e1", White, Queen)
    conf.add("h1", White, Rook)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White) filter { case _: Castle => true case _ => false }
    withClue("When the inside piece was not a king  no castling was considered") {
      moves shouldBe empty
    }
  }

  private def enPassantSelected(): Assertion = {
    val conf: Configuration = new GridConfiguration
    placeKings(conf)
    conf.add("e7", Black, Pawn)
    conf.add("d4", White, Pawn)
    /* Do not allow double advance */
    conf.applyMove(MovePiece("d4", "d5"))
    /* Allow pawn to be captured with en passant */
    conf.applyMove(MovePiece("e7", "e5"))
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White)
    withClue("MovePiece should not have been in the list of acceptable moves") {
      moves should not contain MovePiece("d5", "e6")
    }
    withClue("En passant was in the list of acceptable moves") {
      moves should contain(EnPassant("d5", "e6"))
    }
  }

  private def resignExcluded(): Assertion = {
    val conf: Configuration = new GridConfiguration
    placeKings(conf)
    val e = new StandardMoveExplorer(conf)
    val moves = e.legalMoves(White)
    withClue("Resign was not in the list of legal moves") {
      moves should not contain Resign(White)
    }
  }

  /* legalMoves: end*/

  /* Check Mate start */

  private def kingIsCheckMatedDetected(): Assertion = {
    val conf: Configuration = new GridConfiguration
    placeKings(conf)

    val e1 = new StandardMoveExplorer(conf)
    withClue("The black king was not in check") {
      e1.kingInCheck(Black) shouldBe false
    }
    withClue("The black king was not checkmated") {
      e1.kingInCheckMate(Black) shouldBe false
    }

    conf.add("a8", White, Rook)
    val e2 = new StandardMoveExplorer(conf)
    withClue("The black king was in check") {
      e2.kingInCheck(Black) shouldBe true
    }
    withClue("The black king was not checkmated") {
      e2.kingInCheckMate(Black) shouldBe false
    }

    conf.add("a7", White, Rook)
    val e3 = new StandardMoveExplorer(conf)
    withClue("The black king was in check") {
      e3.kingInCheck(Black) shouldBe true
    }
    withClue("The black king was checkmated") {
      e3.kingInCheckMate(Black) shouldBe true
    }
  }

  /* Check Mate end */

  /* defect: start */

  private def defect5IsFixed(): Assertion = {
    val conf: Configuration = new GridConfiguration
    placeAll(conf)
    for (move <- DefectFixture.defect5Moves) {
      conf.applyMove(move)
    }
    val e = new StandardMoveExplorer(conf)
    val move = DefectFixture.defect5FinalMove
    withClue(s"$move should be rejected") {
      a[InvalidParticipantException] shouldBe thrownBy {
        e.rejectIllegalMove(move)
      }
    }
  }

  /* defect: end */

}
