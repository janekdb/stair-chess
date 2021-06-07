package chess.model

import chess.model.Colours.{Black, White}
import org.scalatest._
import wordspec.AnyWordSpec
import matchers.should.Matchers
import LoneElement._
import chess.test.TestUtils

object GridConfigurationTest extends AnyWordSpec with Matchers with TestUtils {

  "A GridConfiguration" should {
    "behave as expected" in {
      confirmGetRows()
      applied()
    }
    "manage history" in {
      moveHistoryMaintained()
      moveHistoryCopied()
    }
    "manage promotion" in {
      promoteEventsSent()
      promoteCapturingEventsSent()
      promoteReplacesPiece()
    }
    "send en passant event" in {
      enPassantEventSent()
    }
    "send move events" in {
      pieceMovedEventSent()
      pieceMovedCapturingEventSent()
    }
  }

  //noinspection ZeroIndexToHead
  private def confirmGetRows(): Assertion = {
    val conf = new GridConfiguration

    conf.add("a1", White, Pawn)
    conf.add("b2", White, Knight)

    conf.add("d4", Black, Rook)
    conf.add("e4", Black, Queen)

    val rows = conf.getRows
    rows should not be null

    import Constants.BOARD_SIZE

    rows should have size BOARD_SIZE

    all(rows) should have size BOARD_SIZE

    rows(0)(0) shouldBe (White, Pawn)
    rows(1)(1) shouldBe (White, Knight)
    rows(3)(3) shouldBe (Black, Rook)
    rows(3)(4) shouldBe (Black, Queen)

    /* All remaining cells must be null */
    val expectedNullCount = BOARD_SIZE * BOARD_SIZE - 4
    withClue("Null cell count is the total cell count minus the count of non-null cells") {
      rows.flatten.count(_ == null) shouldBe expectedNullCount
    }
  }

  private def moveHistoryMaintained(): Assertion = {
    val conf = new GridConfiguration

    val whiteStart: Position = "e2"
    val whiteEnd: Position   = "e3"
    conf.add(whiteStart, White, Pawn)

    val blackStart: Position = "h8"
    val blackEnd: Position   = "h4"
    conf.add(blackStart, Black, Rook)

    withClue("The last move should have been None") {
      conf.getLastMove shouldBe empty
    }

    conf.applyMove(MovePiece(whiteStart, whiteEnd))
    conf.getLastMove should contain(Pawn, whiteStart, whiteEnd)

    conf.applyMove(MovePiece(blackStart, blackEnd))
    conf.getLastMove should contain(Rook, blackStart, blackEnd)
  }

  private def moveHistoryCopied(): Assertion = {
    val conf = new GridConfiguration

    val whiteStart: Position = "e2"
    val whiteEnd: Position   = "e3"
    conf.add(whiteStart, White, Pawn)

    conf.applyMove(MovePiece(whiteStart, whiteEnd))
    val copy = conf.copyOf
    copy.getLastMove should contain(Pawn, whiteStart, whiteEnd)
  }

  private def enPassantEventSent(): Assertion = {
    val conf = new GridConfiguration

    val whiteStart: Position = "a5"
    val whiteEnd: Position   = "b6"
    conf.add(whiteStart, White, Pawn)

    val blackStart: Position = "b7"
    val blackEnd: Position   = "b5"
    conf.add(blackStart, Black, Pawn)

    conf.applyMove(MovePiece(blackStart, blackEnd))
    val events = conf.applyMove(EnPassant(whiteStart, whiteEnd))
    events match {
      case List(PieceMovedCapturing(start, end, captured)) =>
        start shouldBe whiteStart
        end shouldBe whiteEnd
        captured shouldBe blackEnd
      case _ => fail("Unexpected list of events: " + events)
    }
    val (colour, piece, _) = conf.getExistingPiece(whiteEnd)
    colour shouldBe White
    piece shouldBe Pawn
    withClue("The black pawn should have been taken") {
      conf.getPiece(blackEnd) shouldBe empty
    }
  }

  private def pieceMovedEventSent(): Assertion = {
    val conf                 = new GridConfiguration
    val whiteStart: Position = "a5"
    val whiteEnd: Position   = "a6"
    conf.add(whiteStart, White, Pawn)
    val events = conf.applyMove(MovePiece(whiteStart, whiteEnd))
    withClue("The events sent when a piece was moved should have been correct") {
      events.loneElement shouldBe PieceMoved(whiteStart, whiteEnd)
    }
  }

  private def pieceMovedCapturingEventSent(): Assertion = {
    val conf                 = new GridConfiguration
    val whiteStart: Position = "a5"
    val whiteEnd: Position   = "b6"
    conf.add(whiteStart, White, Pawn)
    conf.add(whiteEnd, Black, Queen)
    val events = conf.applyMove(MovePieceCapturing(whiteStart, whiteEnd))
    withClue("The events sent when a piece captured should have been correct") {
      events.loneElement shouldBe PieceMovedCapturing(whiteStart, whiteEnd, whiteEnd)
    }
  }

  private def promoteEventsSent(): Assertion = {
    val conf                 = new GridConfiguration
    val whiteStart: Position = "a7"
    val whiteEnd: Position   = "a8"
    conf.add(whiteStart, White, Pawn)
    val events = conf.applyMove(Promote(whiteStart, Queen))
    withClue("The events sent when a piece was promoted should have been correct") {
      events shouldBe PieceMoved(whiteStart, whiteEnd) :: Promoted(whiteEnd, Queen) :: Nil
    }
  }

  private def promoteCapturingEventsSent(): Assertion = {
    val conf                 = new GridConfiguration
    val whiteStart: Position = "a7"
    val whiteEnd: Position   = "b8"
    conf.add(whiteStart, White, Pawn)
    conf.add(whiteEnd, Black, Bishop)
    val events = conf.applyMove(PromoteCapturing(whiteStart, whiteEnd, Queen))
    withClue("The events sent when a piece was promote should have been correct") {
      events shouldBe PieceMovedCapturing(whiteStart, whiteEnd, whiteEnd) :: Promoted(whiteEnd, Queen) :: Nil
    }
  }

  private def promoteReplacesPiece(): Assertion = {
    val conf = new GridConfiguration

    val start: Position = "f7"
    val end: Position   = "f8"
    conf.add(start, White, Pawn)
    val events = conf.applyMove(Promote(start, Knight))
    events shouldBe PieceMoved(start, end) :: Promoted(end, Knight) :: Nil
    withClue("There should not have been any pawns") {
      conf.locatePieces(White, Pawn) shouldBe empty
    }
    withClue("A knight should be present") {
      conf.locatePieces(White, Knight).loneElement shouldBe end
    }
  }

  def applied(): Assertion = {
    val conf = new GridConfiguration

    val start: Position = "e4"
    val end: Position   = "e5"
    conf.add(start, White, Pawn)
    val confPost = conf.applied(MovePiece(start, end))

    withClue("The initial configuration was not changed") {
      conf.exists(start) shouldBe true
    }
    withClue("The initial configuration was not changed") {
      conf.locatePieces(White) should have size 1
    }
    withClue("The new configuration was changed") {
      confPost.exists(end) shouldBe true
    }
    withClue("The new configuration had the correct number of pieces") {
      confPost.locatePieces(White).size shouldBe conf.locatePieces(White).size
    }
  }

}
