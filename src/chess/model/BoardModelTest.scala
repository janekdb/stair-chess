package chess.model

import chess.model.Colours.{ Black, White }
import chess.model.ex._

import test.Test
import chess.util.TODO

object BoardModelTest extends Test {

  implicit def piece2list(t: Piece) = List(t)

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

  }

  private def rejectMoveOntoOwnPiece = {
    val bm = new BoardModel

    bm.place(Black, Queen(), new Position("g7"))
    bm.place(Black, King(), new Position("g8"))
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

  private def rejectPawnDoubleAdvanceIfNotFirstMove = {
    val bm = new BoardModel

    bm.place(Black, Pawn(), new Position("a7"))
    bm.place(Black, King(), new Position("e8"))

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

    bm.place(White, Rook(), new Position("h1"))
    bm.place(White, King(), new Position("d1"))
    bm.move(Castle(White, Long))
  }

  private def acceptCastlingWhenIrrelevantOpponentPiecesExist = {
    val bm = new BoardModel

    bm.place(White, Rook(), new Position("h1"))
    bm.place(White, King(), new Position("d1"))
    bm.place(Black, Knight(), new Position("a8"))
    bm.move(Castle(White, Long))
  }

  private def rejectCastlingWhenInterveningPiece = {
    val bm = new BoardModel

    bm.place(White, Rook(), new Position("h1"))
    bm.place(White, King(), new Position("d1"))
    bm.place(White, Bishop(), new Position("e1"))

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
      val bm = new BoardModel

      bm.place(White, Rook(), new Position("h1"))
      bm.place(White, King(), new Position("d1"))
      /* Attack a square */
      bm.place(Black, Rook(), new Position(file + "8"))

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

    bm.place(White, Rook(), new Position("e2"))
    bm.place(White, King(), new Position("e1"))
    bm.place(Black, Rook(), new Position("e7"))

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

    bm.place(White, King(), new Position("a2"))
    bm.place(Black, King(), new Position("h7"))
    bm.place(Black, Rook(), new Position("b8"))
    bm.place(Black, Rook(), new Position("c7"))
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
    
    assert(actual.size == 1, "One BoardChanged event was fired")
    val (colour, winMode) = actual.head
    assert(colour == White)
    assert(winMode == WinModes.CheckMate)
  }
}