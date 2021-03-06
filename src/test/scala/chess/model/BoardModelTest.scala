package chess.model

import chess.model.Colours.{Black, White}
import chess.model.ex._
import org.scalatest._
import LoneElement._
import OptionValues._
import wordspec.AnyWordSpec
import matchers.should.Matchers
import chess.test.TestUtils

class BoardModelTest extends AnyWordSpec with Matchers with Inspectors with TestUtils {

  "A BoardModel" should {
    "reject or accept moves" in {
      rejectMoveOntoOwnPiece()
      rejectPawnDoubleAdvanceIfNotFirstMove()
      acceptCastlingWhenNoInterveningPieces()
      acceptCastlingWhenIrrelevantOpponentPiecesExist()
      rejectCastlingWhenInterveningPiece()
      rejectCastlingWhenAnySquareVisitedByTheKingIsUnderAttack()
      acceptCastlingWhenSquaresVisitedByTheRookButNotTheKingAreUnderAttack()
      rejectReCastling()
      rejectIfMoveLeavesOwnKingInCheck()
      enPassantAllowed()
      enPassantDisallowedIfNotImmediatelyUsed()
    }
    "detect checkmate" in {
      checkMateIsDetected()
    }
    "detect check" in {
      checkWithNonCapturingEscapeIsDetected()
      checkWithCapturingEscapeIsDetected()
    }
    "detect stalemate" in {
      stalemateIsDetected()
      invalidStalemateIsRejected()
    }
    "behave as expected" in {
      /* Configuration event */
      confirmConfigurationEventIsSent()
      // Companion Object
      standardPlacements()
      /* Repeated configurations */
      // TODO: LOW: Allow draw to be claimed
      // repeatedConfigurationsIsDetected

      /* Defects */
      confirmNotResponsibleForDefect5()
      confirmNotResponsibleForDefect6()
    }
  }

  implicit def placementBuilder2List(pb: PlacementsBuilder): List[(Colour, Piece, Position)] = pb.asList

  private class PlacementsBuilder {
    var placements: List[(Colour, Piece, Position)] = Nil

    def apply(colour: Colour, piece: Piece, position: String): Unit = placements =
      (colour, piece, new Position(position)) :: placements

    def apply(placements: List[(Colour, Piece, Position)]): Unit = this.placements = placements ::: this.placements

    def asList: List[(Colour, Piece, Position)] = placements
  }

  private def rejectMoveOntoOwnPiece(): Assertion = {
    val pb = new PlacementsBuilder
    pb(Black, Queen, "g7")
    pb(Black, King, "g8")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    an[UnreachablePositionException] shouldBe thrownBy {
      bm.move("g7g8")
    }
  }

  private def rejectPawnDoubleAdvanceIfNotFirstMove(): Assertion = {
    val pb = new PlacementsBuilder
    pb(Black, Pawn, "a7")
    pb(Black, Queen, "f8")
    pb(getKings)

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    bm.move("a7a6")
    withClue("Pawn double advance should be rejected if it was not the piece's first move") {
      an[UnreachablePositionException] shouldBe thrownBy {
        bm.move("a6a4")
      }
    }
  }

  private def acceptCastlingWhenNoInterveningPieces(): Assertion = {
    val pb = new PlacementsBuilder
    pb(Black, King, "e8")
    pb(White, Rook, "a1")
    pb(White, King, "e1")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    noException shouldBe thrownBy(bm.move(Castle(White, Long)))
  }

  private def acceptCastlingWhenIrrelevantOpponentPiecesExist(): Assertion = {
    val pb = new PlacementsBuilder
    pb(Black, King, "e8")
    pb(White, Rook, "a1")
    pb(White, King, "e1")
    pb(Black, Knight, "a8")

    val bm = new BoardModel(pb, Nil, Nil, Nil)
    noException shouldBe thrownBy(bm.move(Castle(White, Long)))
  }

  private def rejectCastlingWhenInterveningPiece(): Assertion = {
    val pb = new PlacementsBuilder
    pb(White, Rook, "a1")

    pb(White, King, "e1")

    pb(White, Bishop, "c1")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    a[InterveningPieceException] shouldBe thrownBy {
      bm.move(Castle(White, Long))
    }
  }

  private def rejectCastlingWhenAnySquareVisitedByTheKingIsUnderAttack(): Assertion = {

    val files = List("c", "d", "e")
    forAll(files) { file =>
      val pb = new PlacementsBuilder

      /* Allow white to castle long. */
      pb(White, Rook, "a1")
      pb(White, King, "e1")
      /* Attack a square */
      pb(Black, Rook, file + "8")
      pb(Black, King, "h8")

      val bm = new BoardModel(pb, Nil, Nil, Nil)

      withClue("Castling the king over an attacked square should be rejected") {
        a[AttackedPositionException] shouldBe thrownBy {
          bm.move(Castle(White, Long))
        }
      }
    }
  }

  private def acceptCastlingWhenSquaresVisitedByTheRookButNotTheKingAreUnderAttack(): Assertion = {

    val files = List("a", "b")
    forAll(files) { file =>
      val pb = new PlacementsBuilder

      /* Allow white to castle long. */
      pb(White, Rook, "a1")
      pb(White, King, "e1")
      /* Attack a square */
      pb(Black, Rook, file + "8")
      pb(Black, King, "h8")

      val bm = new BoardModel(pb, Nil, Nil, Nil)

      noException shouldBe thrownBy(bm.move(Castle(White, Long)))
    }
  }

  private def rejectReCastling(): Assertion = {
    val pb = new PlacementsBuilder

    pb(White, Rook, "a1")
    pb(White, King, "e1")
    pb(Black, King, "e8")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    bm.move(Castle(White, Long))
    bm.move("e8e7")
    bm.move("d1d2")
    bm.move("e7e8")
    bm.move("d2a2")
    bm.move("e8e7")
    bm.move("a2a1")
    bm.move("c1d1")
    bm.move("e7e8")
    bm.move("d1e1")
    bm.move("e8e7")
    a[PreviouslyMovedException] shouldBe thrownBy {
      bm.move(Castle(White, Long))
    }
  }

  private def rejectIfMoveLeavesOwnKingInCheck(): Assertion = {
    val pb = new PlacementsBuilder

    pb(White, Rook, "e2")
    pb(White, King, "e1")
    pb(Black, Rook, "e7")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    a[CheckedOwnKing] shouldBe thrownBy {
      bm.move("e2h2")
    }
  }

  private def checkMateIsDetected(): Assertion = {
    val pb = new PlacementsBuilder

    pb(White, King, "a2")
    pb(Black, King, "h7")
    pb(Black, Rook, "b8")
    pb(Black, Rook, "c7")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    class VerifyingGameChangedSubscriber(var expectedEvents: List[GameChanged]) extends GameChangedSubscriber {
      def onGameChanged(event: GameChanged): Unit = {
        expectedEvents match {
          case Nil => fail("An event was received when none were expected: " + event)
          case expected :: rest =>
            withClue("Not all expected events were received") {
              event shouldBe expected
            }
            expectedEvents = rest
        }
      }

      def assertAllEventsReceived(): Assertion = expectedEvents shouldBe empty
    }

    val v = new VerifyingGameChangedSubscriber(List(Won(Black, GameOutcomeModes.CheckMate)))
    bm.subscribe(v)
    bm.move("c7a7")
    v.assertAllEventsReceived()
  }

  private def checkWithNonCapturingEscapeIsDetected(): Assertion = {
    val pb = new PlacementsBuilder

    pb(White, King, "b2")
    pb(Black, King, "h7")
    pb(Black, Rook, "c8")
    pb(Black, Rook, "d7")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    var pieceMoved = false
    var eventCount = 0
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(events: List[BoardChanged]): Unit = {
        for (event <- events) {
          eventCount += 1
          event match {
            case _: PieceMoved => pieceMoved = true
            case _             => fail("Unexpected event: " + event)
          }
        }
      }

      def onPiecePlaced(event: PiecePlaced): Unit = {
        fail()
      }
    }

    bm.subscribe(s)

    /* Check the King. This is not checkmate since a move to a2 escapes check. */
    bm.move("d7b7")

    withClue("The game was not won when the king was checked but could escape") {
      pieceMoved shouldBe true
    }
    eventCount shouldBe 1
  }

  //  abcdefgh
  //8 ··Qk····
  //7 ···pb·r·
  //6 ·····n··
  //5 p··P···p
  //4 P······P
  //3 ·Q·n··R·
  //2 ·Rb·KP··
  //1 ········
  //  abcdefgh
  private def checkWithCapturingEscapeIsDetected(): Assertion = {
    val pb = new PlacementsBuilder
    /* Position the Queen so that it can check the black King on the next move. */
    val queenStart = "b7"
    val queenEnd   = "c8"
    pb(White, Queen, queenStart)
    pb(Black, King, "d8")

    pb(Black, Pawn, "d7")
    pb(Black, Bishop, "e7")
    pb(Black, Rook, "g7")

    pb(Black, Knight, "f6")

    pb(Black, Pawn, "a5")
    pb(White, Pawn, "d5")
    pb(Black, Pawn, "h5")

    pb(Black, Pawn, "a4")
    pb(Black, Pawn, "h4")

    pb(White, Queen, "b3")
    pb(Black, Knight, "d3")
    pb(White, Rook, "g3")

    pb(White, Rook, "b2")
    pb(Black, Bishop, "c2")
    pb(White, King, "e2")
    pb(White, Pawn, "f2")

    val bm = new BoardModel(pb, Nil, Nil, Nil)
    val s  = newEventCapturer
    bm.subscribe(s)
    bm.move(MovePiece(queenStart, queenEnd))
    withClue("The list of event should not include Won") {
      s.events should not contain Won(White, GameOutcomeModes.CheckMate)
    }
    withClue("The list of events should be comprised of one PieceMoved event") {
      s.events.loneElement shouldBe PieceMoved(queenStart, queenEnd)
    }
  }

  private def enPassantAllowed(): Assertion = {
    val pb = new PlacementsBuilder

    /* The pawn that will capture via en-passant */
    pb(White, Pawn, "e4")
    pb(Black, Pawn, "d7")

    pb(getKings)

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    var pieceMovedCapturing: Option[PieceMovedCapturing] = None
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(events: List[BoardChanged]): Unit = {
        for (event <- events) {
          event match {
            case e: PieceMovedCapturing => pieceMovedCapturing = Some(e)
            case _                      => fail("Unexpected event: " + event)
          }
        }
      }

      def onPiecePlaced(event: PiecePlaced): Unit = {
        fail()
      }
    }

    bm.move("e4e5")
    /* Double advance on adjacent column with white on the same row allows en passant */
    val blackStart: Position = "d7"
    val blackEnd: Position   = "d5"
    bm.move(MovePiece(blackStart, blackEnd))

    bm.subscribe(s)

    val whiteStart: Position = "e5"
    val whiteEnd: Position   = "d6"
    bm.move(EnPassant(whiteStart, whiteEnd))

    withClue("PieceMovedTaking event was sent") {
      pieceMovedCapturing shouldBe defined
    }
    pieceMovedCapturing.value.start shouldBe whiteStart
    pieceMovedCapturing.value.end shouldBe whiteEnd
    pieceMovedCapturing.value.captured shouldBe blackEnd
  }

  private def enPassantDisallowedIfNotImmediatelyUsed(): Assertion = {
    val pb = new PlacementsBuilder

    pb(getKings)
    /* The pawn that will capture via en-passant */
    pb(White, Pawn, "e4")
    /* The pawn that white will attempt to capture with en-passant */
    pb(Black, Pawn, "d7")

    val bm = new BoardModel(pb, Nil, Nil, Nil)

    bm.move("d7d5")
    bm.move("e4e5")

    /* Black king */
    bm.move("e8d8")

    withClue("En-passant disallowed when not immediately played") {
      /* En-passant */
      an[UnreachablePositionException] shouldBe thrownBy {
        bm.move("e5d6")
      }
    }
  }

  private def confirmConfigurationEventIsSent(): Assertion = {
    var events: List[ConfigurationView] = Nil
    val listener = new Object with ConfigurationChangedSubscriber {
      def onConfigurationChanged(event: ConfigurationView): Unit = {
        events ::= event
      }
    }
    val _ = new BoardModel(List(), Nil, List(listener), Nil)
    events should have size 1
    withClue("The events should have been an instance of ConfigurationView but was: " + events.head.getClass) {
      val filtered = events.collect { case e: ConfigurationView => e }
      filtered should not be empty
    }
  }

  // Companion object

  private def standardPlacements(): Assertion = {
    BoardModel.standardPlacements should have size (4 * 8)
  }

  /* Stalemate */
  private def stalemateIsDetected(): Assertion = {
    val pb = new PlacementsBuilder

    pb(getKings)
    pb(Black, Rook, "d8")
    pb(Black, Rook, "f8")
    pb(Black, Rook, "a2")

    val bm = new BoardModel(pb, Nil, Nil, Nil)
    /* Move black to ensure lastColour is set */
    bm.move("a2b2")

    val s = newGameChangedEventCapturer

    bm.subscribe(s)
    bm.move(None)
    withClue("When no move was offered stalemate was detected") {
      s.events shouldBe Drawn(GameOutcomeModes.Stalemate) :: Nil
    }
    withClue("On stalemate the game is completed") {
      bm.isCompleted shouldBe true
    }
    withClue("On stalemate the game was not won") {
      bm.isWon shouldBe false
    }
    withClue("On stalemate the game was drawn") {
      bm.isDrawn shouldBe true
    }
    withClue("On stalemate the game outcome was stalemate") {
      bm.getGameOutcome.value.isStalemate shouldBe true
    }
    withClue("The winner was not defined when the stalemate was reached") {
      bm.getWinner shouldBe empty
    }
  }

  /* Defend against a Player that fails to select a move when one was available. */
  private def invalidStalemateIsRejected(): Assertion = {
    val pb = new PlacementsBuilder
    pb(getKings)
    val bm = new BoardModel(pb, Nil, Nil, Nil)
    bm.move("e8e7")
    withClue("Invalid  stalemate indication was rejected") {
      an[UnconsideredMovesStalemateException] shouldBe thrownBy {
        bm.move(None)
      }
    }
  }

  /* Repeated configurations */

  /*
The relevant rule in the FIDE laws of chess is 9.2, which reads:

The game is drawn, upon a correct claim by the player having the move, when the same position, for at least the third time
(not necessarily by sequential repetition of moves)
a. is about to appear, if he first writes his move on his scoresheet and declares to the arbiter his intention to make this move, or
b. has just appeared, and the player claiming the draw has the move.
Positions as in (a) and (b) are considered the same, if the same player has the move, pieces of the same kind and colour occupy
the same squares, and the possible moves of all the pieces of both players are the same.
Positions are not [considered to be] the same if a pawn that could have been captured en passant can no longer be captured or
if the right to castle has been changed. (FIDE 2005, Article 9.2)
While the rule does not require that the position occur thrice on nearly consecutive moves, it happens this way very often in practice,
typically with one of the kings being put into perpetual check. The intermediate
positions and moves do not matter · they can be the same or different. The rule applies to positions, not moves.
  private def repeatedConfigurationsIsDetected = fail
   */

  /* Defects */

  private def confirmNotResponsibleForDefect5(): Assertion = {
    val bm = new BoardModel(BoardModel.standardPlacements, Nil, Nil, Nil)
    for (move <- DefectFixture.defect5Moves) {
      bm.move(move)
    }
    val move = DefectFixture.defect5FinalMove
    an[InvalidParticipantException] shouldBe thrownBy {
      bm.move(move)
    }
  }

  private def confirmNotResponsibleForDefect6(): Assertion = {
    val bm = new BoardModel(BoardModel.standardPlacements, Nil, Nil, Nil)
    for (move <- DefectFixture.defect6Moves) {
      bm.move(move)
    }
    val move = DefectFixture.defect6FinalMove
    an[AttackedPositionException] shouldBe thrownBy {
      bm.move(move)
    }
  }

  private def getKings = (White, King, new Position("e1")) :: (Black, King, new Position("e8")) :: Nil

  private class EventCapturer extends BoardChangedSubscriber {
    var events: List[BoardChanged] = Nil

    def onBoardChanged(events: List[BoardChanged]): Unit = {
      this.events = this.events ::: events
    }

    def onPiecePlaced(event: PiecePlaced): Unit = {
      fail()
    }
  }

  private def newEventCapturer = new EventCapturer

  private class GameChangedEventCapturer extends GameChangedSubscriber {
    var events: List[GameChanged] = Nil

    def onGameChanged(event: GameChanged): Unit = {
      events = events ::: List(event)
    }
  }

  private def newGameChangedEventCapturer = new GameChangedEventCapturer

}
