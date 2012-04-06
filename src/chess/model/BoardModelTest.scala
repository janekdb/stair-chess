package chess.model

import Colours.{ Black, White }
import ex._
import test.{Main, Test, TestUtils}
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
    checkButNotMateIsDetected
    enPassantAllowed
    enPassantDisallowedIfNotImmediatelyUsed
    // Companion Object
    standardPlacements

  }

  implicit def placementBuilder2List(pb: PlacementsBuilder) = pb.asList

  private class PlacementsBuilder {
    var placements: List[(Colour, Piece, Position)] = Nil
    def apply(colour: Colour, piece: Piece, position: String) =  placements = (colour, piece, new Position(position)) :: placements
    def apply(placements: List[(Colour, Piece, Position)]) = this.placements = placements ::: this.placements
    def asList = placements
  }
    
  private def rejectMoveOntoOwnPiece {
    val pb = new PlacementsBuilder
    pb(Black, Queen(), "g7")
    pb(Black, King(), "g8")

    val bm = new BoardModel(pb, Nil)

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

    val bm = new BoardModel(pb, Nil)

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

    val bm = new BoardModel(pb, Nil)

    bm.move(Castle(White, Long))
  }

  private def acceptCastlingWhenIrrelevantOpponentPiecesExist {
    val pb = new PlacementsBuilder
    pb(Black, King(), "e8") 
    pb(White, Rook(), "a1") 
    pb(White, King(), "e1") 
    pb(Black, Knight(), "a8") 

    val bm = new BoardModel(pb, Nil)
    bm.move(Castle(White, Long))
  }

  private def rejectCastlingWhenInterveningPiece {
    val pb = new PlacementsBuilder
    pb(White, Rook(), "a1") 

    pb(White, King(), "e1") 

    pb(White, Bishop(), "c1") 

    val bm = new BoardModel(pb, Nil)
    
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

      val bm = new BoardModel(pb, Nil)

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

      val bm = new BoardModel(pb, Nil)

      bm.move(Castle(White, Long))
    }
  }

  private def rejectReCastling {
    val pb = new PlacementsBuilder

    pb(White, Rook(), "a1")
    pb(White, King(), "e1")
    pb(Black, King(), "e8")

    val bm = new BoardModel(pb, Nil)

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

    val bm = new BoardModel(pb, Nil)

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

    val bm = new BoardModel(pb, Nil)
    
    // TODO: Convert this test to use a verifying BoardChangedSubscriber possibly a mock
    var actual: List[(Colour, WinModes.WinMode)] = Nil

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
    assertEquals(WinModes.CheckMate, winMode)
  }

  private def checkButNotMateIsDetected {
    val pb = new PlacementsBuilder

    pb(White, King(), "b2") 
    pb(Black, King(), "h7") 
    pb(Black, Rook(), "c8") 
    pb(Black, Rook(), "d7") 

    val bm = new BoardModel(pb, Nil)

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

  private def enPassantAllowed {
    val pb = new PlacementsBuilder

    /* The pawn that will capture via en-passant */
    pb(White, Pawn(), "e4") 
    pb(Black, Pawn(), "d7") 

    pb(getKings)

    val bm = new BoardModel(pb, Nil)

    var pieceMovedTaking: PieceMovedTaking = null
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) {
        event match {
          case e @ PieceMovedTaking(_, _, _) => pieceMovedTaking = e
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

    assertNotNull(pieceMovedTaking, "PieceMovedTaking event was sent")
    assertEquals(whiteStart, pieceMovedTaking.start, "PieceMovedTaking.start was correct: " + pieceMovedTaking)
    assertEquals(whiteEnd, pieceMovedTaking.end, "PieceMovedTaking.end was correct: " + pieceMovedTaking)
    assertEquals(blackEnd, pieceMovedTaking.taken, "PieceMovedTaking.taken was correct: Expected: " + blackEnd + ", had: " + pieceMovedTaking)
  }

  private def enPassantDisallowedIfNotImmediatelyUsed {
    val pb = new PlacementsBuilder

    pb(getKings)
    /* The pawn that will capture via en-passant */
    pb(White, Pawn(), "e4") 
    /* The pawn that white will attempt to capture with en-passant */
    pb(Black, Pawn(), "d7") 

    val bm = new BoardModel(pb, Nil)

    bm.move("d7d5")
    bm.move("e4e5")

    /* Black king */
    bm.move("e8d8")
    
    assertExceptionThrown("En-passant disallowed when not immediately played", classOf[UnreachablePositionException] ) {
      /* En-passant */
      bm.move("e5d6")
    }

  }
  
  // Companion object
  
  private def standardPlacements {
    assertEquals(4 * 8, BoardModel.standardPlacements.size, "There was a correct number of placements")
  }

  private def getKings = (White, King(), new Position("e1")) :: (Black, King(), new Position("e8")):: Nil

}