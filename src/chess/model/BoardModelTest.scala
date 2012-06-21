package chess.model

import Colours.{ Black, White }
import GameOutcomeModes.GameOutcomeMode
import ex._
import test.{ Main, Test, TestUtils }
import chess.util.TODO
import scala.collection.mutable.ListBuffer
import chess.ui.UI

object BoardModelTest extends Test with TestUtils with Main {

  def runTests {

    rejectMoveOntoOwnPiece
    rejectPawnDoubleAdvanceIfNotFirstMove
    acceptCastlingWhenNoInterveningPieces
    acceptCastlingWhenIrrelevantOpponentPiecesExist
    rejectCastlingWhenInterveningPiece
    rejectCastlingWhenAnySquareVisitedByTheKingIsUnderAttack
    acceptCastlingWhenSquaresVisitedByTheRookButNotTheKingAreUnderAttack
    rejectReCastling
    rejectIfMoveLeavesOwnKingInCheck
    checkMateIsDetected
    checkWithNonCapturingEscapeIsDetected
    checkWithCapturingEscapeIsDetected
    enPassantAllowed
    enPassantDisallowedIfNotImmediatelyUsed
    /* Configuration event */
    confirmConfigurationEventIsSent
    // Companion Object
    standardPlacements
    /* Stalemate */
    stalemateIsDetected
    invalidStalemateIsRejected
    /* Repeated configurations */
    // TODO: LOW: Allow draw to be claimed
    // repeatedConfigurationsIsDetected

  }

  implicit def placementBuilder2List(pb: PlacementsBuilder) = pb.asList

  private class PlacementsBuilder {
    var placements: List[(Colour, Piece, Position)] = Nil
    def apply(colour: Colour, piece: Piece, position: String) = placements = (colour, piece, new Position(position)) :: placements
    def apply(placements: List[(Colour, Piece, Position)]) = this.placements = placements ::: this.placements
    def asList = placements
  }

  private def rejectMoveOntoOwnPiece {
    val pb = new PlacementsBuilder
    pb(Black, Queen(), "g7")
    pb(Black, King(), "g8")

    val bm = new BoardModel(pb, Nil, Nil)

    try {
      bm.move("g7g8")
      fail("Move onto own piece should be rejected")
    } catch {
      /* Success */
      case e: UnreachablePositionException => Unit
      /* Unexpected */
      case e: Exception => unexpected(e)
    }
  }

  private def rejectPawnDoubleAdvanceIfNotFirstMove {
    val pb = new PlacementsBuilder
    pb(Black, Pawn(), "a7")
    pb(Black, Queen(), "f8")
    pb(getKings)

    val bm = new BoardModel(pb, Nil, Nil)

    bm.move("a7a6")
    try {
      bm.move("a6a4")
      fail("Pawn double advance should be rejected if it was not the piece's first move")
    } catch {
      /* Success */
      case _: UnreachablePositionException => Unit
      /* Unexpected */
      case e: Exception => unexpected(e)
    }
  }

  private def acceptCastlingWhenNoInterveningPieces {
    val pb = new PlacementsBuilder
    pb(Black, King(), "e8")
    pb(White, Rook(), "a1")
    pb(White, King(), "e1")

    val bm = new BoardModel(pb, Nil, Nil)

    bm.move(Castle(White, Long))
  }

  private def acceptCastlingWhenIrrelevantOpponentPiecesExist {
    val pb = new PlacementsBuilder
    pb(Black, King(), "e8")
    pb(White, Rook(), "a1")
    pb(White, King(), "e1")
    pb(Black, Knight(), "a8")

    val bm = new BoardModel(pb, Nil, Nil)
    bm.move(Castle(White, Long))
  }

  private def rejectCastlingWhenInterveningPiece {
    val pb = new PlacementsBuilder
    pb(White, Rook(), "a1")

    pb(White, King(), "e1")

    pb(White, Bishop(), "c1")

    val bm = new BoardModel(pb, Nil, Nil)

    try {
      bm.move(Castle(White, Long))
      fail("Castling should be rejected when there is an intervening piece")
    } catch {
      case _: InterveningPieceException => Unit
      case e: Exception => unexpected(e)
    }
  }

  private def rejectCastlingWhenAnySquareVisitedByTheKingIsUnderAttack {

    val files = List("c", "d", "e");
    for (file <- files) {
      val pb = new PlacementsBuilder

      /* Allow white to castle long. */
      pb(White, Rook(), "a1")
      pb(White, King(), "e1")
      /* Attack a square */
      pb(Black, Rook(), file + "8")
      pb(Black, King(), "h8")

      val bm = new BoardModel(pb, Nil, Nil)

      assertExceptionThrown("Castling the king over an attacked square should be rejected", classOf[AttackedPositionException]) {
        bm.move(Castle(White, Long))
      }
    }
  }

  private def acceptCastlingWhenSquaresVisitedByTheRookButNotTheKingAreUnderAttack {

    val files = List("a", "b");
    for (file <- files) {
      val pb = new PlacementsBuilder

      /* Allow white to castle long. */
      pb(White, Rook(), "a1")
      pb(White, King(), "e1")
      /* Attack a square */
      pb(Black, Rook(), file + "8")
      pb(Black, King(), "h8")

      val bm = new BoardModel(pb, Nil, Nil)

      bm.move(Castle(White, Long))
    }
  }

  private def rejectReCastling {
    val pb = new PlacementsBuilder

    pb(White, Rook(), "a1")
    pb(White, King(), "e1")
    pb(Black, King(), "e8")

    val bm = new BoardModel(pb, Nil, Nil)

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
    assertExceptionThrown("Re-Castling was rejected", classOf[PreviouslyMovedException]) {
      bm.move(Castle(White, Long))
    }
  }

  private def rejectIfMoveLeavesOwnKingInCheck {
    val pb = new PlacementsBuilder

    pb(White, Rook(), "e2")
    pb(White, King(), "e1")
    pb(Black, Rook(), "e7")

    val bm = new BoardModel(pb, Nil, Nil)

    try {
      bm.move("e2h2")
      fail("A move by white that leaves the whte King in check should be rejected")
    } catch {
      case _: CheckedOwnKing => Unit
      case e: Exception => unexpected(e)
    }
  }

  private def checkMateIsDetected {
    val pb = new PlacementsBuilder

    pb(White, King(), "a2")
    pb(Black, King(), "h7")
    pb(Black, Rook(), "b8")
    pb(Black, Rook(), "c7")

    val bm = new BoardModel(pb, Nil, Nil)

    // TODO: Convert this test to use a verifying BoardChangedSubscriber possibly a mock
    var actual: List[(Colour, GameOutcomeModes.GameOutcomeMode)] = Nil

    val s = new BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) {
        event match {
          case Won(colour, wonMode) => actual = (colour, wonMode) :: actual
          case default => Unit
        }
      }
    }

    bm.subscribe(s)

    bm.move("c7a7")

    assertEquals(1, actual.size, "One BoardChanged event was fired: " + actual.size)
    val (colour, winMode) = actual.head
    assertEquals(Black, colour)
    assertEquals(GameOutcomeModes.CheckMate, winMode)
  }

  private def checkWithNonCapturingEscapeIsDetected {
    val pb = new PlacementsBuilder

    pb(White, King(), "b2")
    pb(Black, King(), "h7")
    pb(Black, Rook(), "c8")
    pb(Black, Rook(), "d7")

    val bm = new BoardModel(pb, Nil, Nil)

    var pieceMoved = false
    var eventCount = 0
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) {
        eventCount += 1
        event match {
          case PieceMoved(_, _) => pieceMoved = true
          case default => fail("Unexpected event: " + event)
        }
      }
    }

    bm.subscribe(s)

    /* Check the King. This is not checkmate since a move to a2 escapes check. */
    bm.move("d7b7")

    assert(pieceMoved, "The game was not won when the king was checked but could escape")
    assertEquals(1, eventCount, "There was only one event")
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
  private def checkWithCapturingEscapeIsDetected {
    val pb = new PlacementsBuilder
    /* Position the Queen so that it can check the black King on the next move. */
    val queenStart = "b7"
    val queenEnd = "c8"
    pb(White, Queen(), queenStart)
    pb(Black, King(), "d8")

    pb(Black, Pawn(), "d7")
    pb(Black, Bishop(), "e7")
    pb(Black, Rook(), "g7")

    pb(Black, Knight(), "f6")

    pb(Black, Pawn(), "a5")
    pb(White, Pawn(), "d5")
    pb(Black, Pawn(), "h5")

    pb(Black, Pawn(), "a4")
    pb(Black, Pawn(), "h4")

    pb(White, Queen(), "b3")
    pb(Black, Knight(), "d3")
    pb(White, Rook(), "g3")

    pb(White, Rook(), "b2")
    pb(Black, Bishop(), "c2")
    pb(White, King(), "e2")
    pb(White, Pawn(), "f2")

    val bm = new BoardModel(pb, Nil, Nil)
    val s = newEventCapturer
    bm.subscribe(s)
    bm.move(MovePiece(queenStart, queenEnd))
    assertFalse(s.events contains Won(White, GameOutcomeModes.CheckMate), "The list of event should not include Won")
    assertEquals(List(PieceMoved(queenStart, queenEnd)), s.events, "The list of events should be comprised of one PieceMoved event")
  }

  private def enPassantAllowed {
    val pb = new PlacementsBuilder

    /* The pawn that will capture via en-passant */
    pb(White, Pawn(), "e4")
    pb(Black, Pawn(), "d7")

    pb(getKings)

    val bm = new BoardModel(pb, Nil, Nil)

    var pieceMovedCapturing: PieceMovedCapturing = null
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) {
        event match {
          case e @ PieceMovedCapturing(_, _, _) => pieceMovedCapturing = e
          case default => fail("Unexpected event: " + event)
        }
      }
    }

    bm.move("e4e5")
    /* Double advance on adjacent column with white on the same row allows en passant */
    val blackStart: Position = "d7"
    val blackEnd: Position = "d5"
    bm.move(MovePiece(blackStart, blackEnd))

    bm.subscribe(s)

    val whiteStart: Position = "e5"
    val whiteEnd: Position = "d6"
    bm.move(EnPassant(whiteStart, whiteEnd))

    assertNotNull(pieceMovedCapturing, "PieceMovedTaking event was sent")
    assertEquals(whiteStart, pieceMovedCapturing.start, "PieceMovedCapturing.start was correct: " + pieceMovedCapturing)
    assertEquals(whiteEnd, pieceMovedCapturing.end, "PieceMovedCapturing.end was correct: " + pieceMovedCapturing)
    assertEquals(blackEnd, pieceMovedCapturing.captured, "PieceMovedCapturing.captured was correct: Expected: " + blackEnd + ", had: " + pieceMovedCapturing)
  }

  private def enPassantDisallowedIfNotImmediatelyUsed {
    val pb = new PlacementsBuilder

    pb(getKings)
    /* The pawn that will capture via en-passant */
    pb(White, Pawn(), "e4")
    /* The pawn that white will attempt to capture with en-passant */
    pb(Black, Pawn(), "d7")

    val bm = new BoardModel(pb, Nil, Nil)

    bm.move("d7d5")
    bm.move("e4e5")

    /* Black king */
    bm.move("e8d8")

    assertExceptionThrown("En-passant disallowed when not immediately played", classOf[UnreachablePositionException]) {
      /* En-passant */
      bm.move("e5d6")
    }
  }

  private def confirmConfigurationEventIsSent {
    var events: List[ConfigurationView] = Nil
    val listener = new Object with ConfigurationChangedSubscriber {
      def onConfigurationChanged(event: ConfigurationView) {
        events ::= event
      }
    }
    val bm = new BoardModel(List(), Nil, List(listener))
    assertEquals(1, events.size, "The list of recieved events should have had one element")
    assertTrue(events(0).isInstanceOf[ConfigurationView], "The events should have been an instance of ConfigurationView but was: " + events(0).getClass)
  }

  // Companion object

  private def standardPlacements {
    assertEquals(4 * 8, BoardModel.standardPlacements.size, "There was a correct number of placements")
  }

  /* Stalemate */
  private def stalemateIsDetected {
    val pb = new PlacementsBuilder

    pb(getKings)
    pb(Black, Rook(), "d8")
    pb(Black, Rook(), "f8")
    pb(Black, Rook(), "a2")

    val bm = new BoardModel(pb, Nil, Nil)
    /* Move black to ensure lastColour is set */
    bm.move("a2b2")

    val s = newEventCapturer

    bm.subscribe(s)
    bm.move(None)
    assertEquals(List(Drawn(GameOutcomeModes.Stalemate)), s.events, "When no move was offered stalemate was detected")
    assertTrue(bm.isCompleted, "On stalemate the game is completed")
    assertFalse(bm.isWon, "On stalemate the game was not won")
    assertTrue(bm.isDrawn, "On stalemate the game was drawn")
    assertTrue(bm.getGameOutcome.get.isStalemate, "On stalemate the game outcome was stalemate")
    assertTrue(bm.getWinner.isEmpty, "The winner was not defined when the stalemate was reached")
  }

  /* Defend against a Player that fails to select a move when one was available. */
  private def invalidStalemateIsRejected {
    val pb = new PlacementsBuilder
    pb(getKings)
    val bm = new BoardModel(pb, Nil, Nil)
    bm.move("e8e7")
    assertExceptionThrown("Invalid  stalemate indication was rejected", classOf[UnconsideredMovesStalemateException]) {
          bm.move(None)
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
positions and moves do not matter – they can be the same or different. The rule applies to positions, not moves.
  private def repeatedConfigurationsIsDetected = fail
  */

  private def getKings = (White, King(), new Position("e1")) :: (Black, King(), new Position("e8")) :: Nil

  private def newEventCapturer = new Object with BoardChangedSubscriber {
    var events: List[BoardChanged] = Nil
    def onBoardChanged(event: BoardChanged) {
      events = events ::: List(event)
    }
  }

}