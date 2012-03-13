package chess.model

import Colours.{ Black, White }
import ex._
import test.Test
import chess.util.TODO
import scala.collection.mutable.ListBuffer

object BoardModelTest extends Test {

  // TODO: Define this test helping implicits in a common location
  implicit def piece2List(t: Piece) = List(t)
  implicit def string2Position(s: String) = new Position(s)
  implicit def string2MovePiece(s: String) = new MovePiece(s)
  
  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests(): Unit = {

    rejectMoveOntoOwnPiece
    rejectPawnDoubleAdvanceIfNotFirstMove
    acceptCastlingWhenNoInterveningPieces
    acceptCastlingWhenIrrelevantOpponentPiecesExist
    rejectCastlingWhenInterveningPiece
    rejectCastlingWhenAnySquareUnderAttack
    rejectIfMoveLeavesOwnKingInCheck
    checkMateIsDetected
    checkButNotMateIsDetected
    enPassantAllowed
    enPassantDisallowedIfNotImmediatelyUsed

  }

  private def rejectMoveOntoOwnPiece = {
    // TODO: Use a more compact syntax for building the placement list
    var placements: List[(Colour, Piece, Position)] = List()
    placements = (Black, Queen(), new Position("g7")) :: placements
    placements = (Black, King(), new Position("g8")) :: placements

    val bm = new BoardModel(placements)

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

  private def rejectPawnDoubleAdvanceIfNotFirstMove = {
    var placements: List[(Colour, Piece, Position)] = List()
    placements = (Black, Pawn(), new Position("a7")) :: placements
    placements = (Black, Queen(), new Position("f8")) :: placements
    placements = placements ::: getKings

    val bm = new BoardModel(placements)

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

  private def acceptCastlingWhenNoInterveningPieces = {
    var placements: List[(Colour, Piece, Position)] = List()
    placements = (Black, King(), new Position("e8")) :: placements
    placements = (White, Rook(), new Position("h1")) :: placements
    placements = (White, King(), new Position("d1")) :: placements

    val bm = new BoardModel(placements)

    bm.move(Castle(White, Long))
  }

  private def acceptCastlingWhenIrrelevantOpponentPiecesExist = {
    var placements: List[(Colour, Piece, Position)] = List()
    placements = (Black, King(), new Position("e8")) :: placements
    placements = (White, Rook(), new Position("h1")) :: placements
    placements = (White, King(), new Position("d1")) :: placements
    placements = (Black, Knight(), new Position("a8")) :: placements

    val bm = new BoardModel(placements)
    bm.move(Castle(White, Long))
  }

  private def rejectCastlingWhenInterveningPiece = {
    var placements: List[(Colour, Piece, Position)] = List()
    placements = (White, Rook(), new Position("h1")) :: placements

    placements = (White, King(), new Position("d1")) :: placements

    placements = (White, Bishop(), new Position("e1")) :: placements

    val bm = new BoardModel(placements)
    
    try {
      bm.move(Castle(White, Long))
      fail("Castling should be rejected when there is an intervening piece")
    } catch {
      case _: InterveningPieceException => Unit
      case e: Exception => unexpected(e)
    }
  }

  private def rejectCastlingWhenAnySquareUnderAttack = {

    val files = List("d", "e", "f", "g", "h");
    for (file <- files) {
      var placements: List[(Colour, Piece, Position)] = List()

      placements = (White, Rook(), new Position("h1")) :: placements
      placements = (White, King(), new Position("d1")) :: placements
      /* Attack a square */
      placements = (Black, Rook(), new Position(file + "8")) :: placements
      //      bm.place(Black, Rook(), file + "8)

      val bm = new BoardModel(placements)

      try {
        bm.move(Castle(White, Long))
        fail("Castling over an attacked square should be rejected")
      } catch {
        case _: AttackedPositionException => Unit
        case e: Exception => unexpected(e)
      }
    }
  }

  private def rejectIfMoveLeavesOwnKingInCheck = {
    var placements: List[(Colour, Piece, Position)] = List()

    placements = (White, Rook(), new Position("e2")) :: placements
    placements = (White, King(), new Position("e1")) :: placements
    placements = (Black, Rook(), new Position("e7")) :: placements

    val bm = new BoardModel(placements)

    try {
      bm.move("e2h2")
      fail("A move by white that leaves the whte King in check should be rejected")
    } catch {
      case _: CheckedOwnKing => Unit
      case e: Exception => unexpected(e)
    }
  }

  private def checkMateIsDetected = {
    var placements: List[(Colour, Piece, Position)] = List()

    placements = (White, King(), new Position("a2")) :: placements
    placements = (Black, King(), new Position("h7")) :: placements
    placements = (Black, Rook(), new Position("b8")) :: placements
    placements = (Black, Rook(), new Position("c7")) :: placements

    val bm = new BoardModel(placements)
    
    // TODO: Write this test without assignment to this var maybe by using a pattern guard in the subscriber to fall through
    var actual: List[(Colour, WinModes.WinMode)] = List()

    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) = {
        event match {
          case Won(colour, wonMode) => {
            actual = (colour, wonMode) :: actual
          }
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

  private def checkButNotMateIsDetected = {
    var placements: List[(Colour, Piece, Position)] = List()

    placements = (White, King(), new Position("b2")) :: placements
    placements = (Black, King(), new Position("h7")) :: placements
    placements = (Black, Rook(), new Position("c8")) :: placements
    placements = (Black, Rook(), new Position("d7")) :: placements

    val bm = new BoardModel(placements)

    var pieceMoved = false
    var eventCount = 0
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) = {
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

  private def enPassantAllowed = {
    var placements: List[(Colour, Piece, Position)] = List()

    /* The pawn that will capture via en-passant */
    placements = (White, Pawn(), new Position("e4")) :: placements
    placements = (Black, Pawn(), new Position("d7")) :: placements

    // TODO: Initialize list as getKings
    placements = placements ::: getKings

    val bm = new BoardModel(placements)

    var pieceMovedTaking: PieceMovedTaking = null
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) = {
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

  private def enPassantDisallowedIfNotImmediatelyUsed = {

    var placements: List[(Colour, Piece, Position)] = List()

    placements = placements ::: getKings
    /* The pawn that will capture via en-passant */
    placements = (White, Pawn(), new Position("e4")) :: placements
    /* The pawn that white will attempt to capture with en-passant */
    placements = (Black, Pawn(), new Position("d7")) :: placements

    val bm = new BoardModel(placements)

    bm.move("d7d5")
    bm.move("e4e5")

    /* Black king */
    bm.move("e8d8")

    // TODO: Replace with block and expected exception class taking help method
    try {
      /* En-passant */
      bm.move("e5d6")
      fail("En-passant disallowed when not immediately played")
    } catch {
      case e: UnreachablePositionException => { /* Expected */ }
      case e: Exception => unexpected(e)
    }
  }

  private def getKings = (White, King(), new Position("e1")) :: (Black, King(), new Position("e8")):: Nil

}