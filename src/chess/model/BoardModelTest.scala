package chess.model

import chess.model.Colours.{ Black, White }
import chess.model.ex._

import test.Test
import chess.util.TODO

object BoardModelTest extends Test {

  implicit def piece2List(t: Piece) = List(t)
  implicit def string2Position(s: String) = new Position(s)
  // TODO: Replace new MovePiece(x) with x
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
    val bm = new BoardModel

    bm.place(Black, Queen(), "g7")
    bm.place(Black, King(), "g8")
    // TODO: Stop direct access to this property.
    bm.placed = true
    try {
      bm.move(new MovePiece("g7g8"))
      fail("Move onto own piece should be rejected")
    } catch {
      /* Success */
      case e: UnreachablePositionException => Unit
      /* Unexpected */
      case e: Exception => unexpected(e)
    }
  }

  private def placeKings(boardModel: BoardModel) = {
    boardModel.place(White, King(), "e1")
    boardModel.place(Black, King(), "e8")
  }

  private def rejectPawnDoubleAdvanceIfNotFirstMove = {
    val bm = new BoardModel

    placeKings(bm)

    bm.place(Black, Pawn(), "a7")
    bm.place(Black, Queen(), "f8")

    // TODO: Stop direct access to this property.
    bm.placed = true
    bm.move(new MovePiece("a7a6"))
    try {
      bm.move(new MovePiece("a6a4"))
      fail("Pawn double advance should be rejected if it was not the piece's first move")
    } catch {
      /* Success */
      case _: UnreachablePositionException => Unit
      /* Unexpected */
      case e: Exception => unexpected(e)
    }
  }

  private def acceptCastlingWhenNoInterveningPieces = {
    val bm = new BoardModel
    bm.place(Black, King(), "e8")

    bm.place(White, Rook(), "h1")
    bm.place(White, King(), "d1")
    bm.move(Castle(White, Long))
  }

  private def acceptCastlingWhenIrrelevantOpponentPiecesExist = {
    val bm = new BoardModel
    bm.place(Black, King(), "e8")
    bm.place(White, Rook(), "h1")
    bm.place(White, King(), "d1")
    bm.place(Black, Knight(), "a8")
    bm.move(Castle(White, Long))
  }

  private def rejectCastlingWhenInterveningPiece = {
    val bm = new BoardModel

    bm.place(White, Rook(), "h1")
    bm.place(White, King(), "d1")
    bm.place(White, Bishop(), "e1")

    try {
      bm.move(Castle(White, Long))
      fail("Castling should be rejected when there is an intervening piece")
    } catch {
      case _: InterveningPieceException => Unit
      case e: Exception => unexpected(e)
    }
  }

  // TODO: Add to Evernote: Multiple consoles
  // TODO: Add to Evernote: Use of OSGi console in Eclipse - can connect to GF?

  private def rejectCastlingWhenAnySquareUnderAttack = {

    val files = List("d", "e", "f", "g", "h");
    for (file <- files) {
      val bm = new BoardModel

      bm.place(White, Rook(), "h1")
      bm.place(White, King(), "d1")
      /* Attack a square */
      bm.place(Black, Rook(), file + "8")

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
    val bm = new BoardModel

    bm.place(White, Rook(), "e2")
    bm.place(White, King(), "e1")
    bm.place(Black, Rook(), "e7")

    try {
      bm.move(new MovePiece("e2h2"))
      fail("A move by white that leaves the whte King in check should be rejected")
    } catch {
      case _: CheckedOwnKing => Unit
      case e: Exception => unexpected(e)
    }
  }

  private def checkMateIsDetected = {
    val bm = new BoardModel

    bm.place(White, King(), "a2")
    bm.place(Black, King(), "h7")
    bm.place(Black, Rook(), "b8")
    bm.place(Black, Rook(), "c7")
    // TODO: Write this test without assignment to this vars maybe by using a pattern guard in the subscriber to fall through
    var actual: List[(Colour, WinModes.WinMode)] = List()

    //    
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

    bm.move(new MovePiece("c7a7"))

    assert(actual.size == 1, "One BoardChanged event was fired: " + actual.size)
    val (colour, winMode) = actual.head
    assert(colour == Black)
    assert(winMode == WinModes.CheckMate)
  }

  private def checkButNotMateIsDetected = {
    val bm = new BoardModel

    bm.place(White, King(), "b2")
    bm.place(Black, King(), "h7")
    bm.place(Black, Rook(), "c8")
    bm.place(Black, Rook(), "d7")

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
    bm.move(new MovePiece("d7b7"))

    assert(pieceMoved, "The game was not won when the king was checked but could escape")
    assert(eventCount == 1, "There was only one event")
  }

  private def enPassantAllowed = {
    val bm = newBoardModel

    /* The pawn that will capture via en-passant */
    bm.place(White, Pawn(), "e2")
    bm.place(Black, Pawn(), "d7")

    // TODO: Find out how to declare a null local var of type PieceMovedTaking but
    //  then do not use it because the List mechanism is cleaner
    var pieceMovedTaking = List[PieceMovedTaking]()
    val s = new Object with BoardChangedSubscriber {
      def onBoardChanged(event: BoardChanged) = {
        event match {
          case e @ PieceMovedTaking(_, _, _) => pieceMovedTaking = List(e)
          case default => fail("Unexpected event: " + event)
        }
      }
    }

    bm.move("e2e4")
    bm.move("e4e5")
    
    bm.move("d7d5")

    bm.subscribe(s)
    bm.move("e5d6")

    assert(pieceMovedTaking == PieceMovedTaking("d6", "e5", "d5"), "Black's pawn was taken via en passant")
  }

  private def enPassantDisallowedIfNotImmediatelyUsed = {
    fail
  }

  private def newBoardModel = {
    val bm = new BoardModel

    bm.place(White, King(), "e1")
    bm.place(Black, King(), "e8")
    bm
  }
}