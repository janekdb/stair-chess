package chess.player

import test.{ Main, Test, TestUtils }
import chess.model._
import chess.model.Colours._

object RandomPlayerTest extends Test with TestUtils with Main {

  def runTests {
    canMove
    isRandom
    selectsOnlyMove
    pawnPromotionSelected
    queenCaptureSelected
    queenCaptureSelected2
    shortCastlingSelected
    longCastlingSelected
    shortCastlingNotConsiderWhenStartPositionsIncorrect
    longCastlingNotConsiderWhenStartPositionsIncorrect
  }

  private def canMove {
    val conf: Configuration = new GridConfiguration
    addKing(conf)
    conf.add("a1", White, Rook())
    val rp = newRandomPlayer(conf)
    assertNotNull(rp.getMove, "A move should be available")
  }

  private def isRandom {
    val conf: Configuration = new GridConfiguration
    addKing(conf)
    conf.add("a1", White, Rook())
    val rp = newRandomPlayer(conf)
    val m1 = rp.getMove
    conf.applyMove(m1)
    val m2 = rp.getMove
    assertNotEquals(m1, m2, "The second move should be different to the first")
  }

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

    val rp = newRandomPlayer(conf)
    val m = rp.getMove
    assertEquals(MovePieceCapturing("a8", "b8"), m, "The only possible move should have been selected")
  }

  private def pawnPromotionSelected {
    val conf: Configuration = new GridConfiguration
    /* The pawn that should be promoted */
    conf.add("b7", White, Pawn())
    /* Box the White king in */
    conf.add("h8", White, King());
    conf.add("h7", White, Pawn());
    conf.add("g8", White, Pawn());
    conf.add("g7", White, Pawn());

    val rp = newRandomPlayer(conf)
    val m = rp.getMove
    m match {
      case mp: Promote =>
        assertEquals(Promote("b7", "b8", Queen()), mp.copy(piece = Queen()), "Pawn promotion was selected")
      case default => fail("Move was not Promote: " + m)
    }
  }

  //  abcdefgh
  //8 ��������
  //7 ��������
  //6 ������q�
  //5 ��������
  //4 �K������
  //3 ���kQ���
  //2 �������Q
  //1 ��.b����
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
    val rp = newRandomPlayer(conf, Black)
    val m = rp.getMove
    assertEquals(MovePieceCapturing("d3", "e3"), m, "Black escaped from check by selected the only possible move")
  }

//  abcdefgh
//8 ������k�
//7 �����Q��
//6 ��������
//5 ��������
//4 ���B����
//3 pR�p�p��
//2 �����P��
//1 ������K�
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
    val rp = newRandomPlayer(conf, Black)
    val m = rp.getMove
    assertEquals(MovePieceCapturing("g8", "f7"), m, "Black escaped from check by selected the only possible move")
  }

  private def shortCastlingSelected {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook())
    conf.add("e1", White, King())
    conf.add("h1", White, Rook())
    val allowedMove = Castle(White, Short)
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    val rp = new RandomPlayer(White, conf, explorer) {
      override protected def moveAcceptable(move: Move): Boolean = {
        move == allowedMove
      }
    }
    val m = rp.getMove
    assertEquals(allowedMove, m, "When the only possible move was short castling it was selected")
  }

  private def longCastlingSelected {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook())
    conf.add("e1", White, King())
    conf.add("h1", White, Rook())
    val allowedMove = Castle(White, Long)
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    val rp = new RandomPlayer(White, conf, explorer) {
      override protected def moveAcceptable(move: Move): Boolean = {
        move == allowedMove
      }
    }
    val m = rp.getMove
    assertEquals(allowedMove, m, "When the only possible move was long castling it was selected")
  }

  private def shortCastlingNotConsiderWhenStartPositionsIncorrect {
    val conf: Configuration = new GridConfiguration
    conf.add("a1", White, Rook())
    conf.add("e1", White, King())
    conf.add("h2", White, Rook())
    var castlingMoves = List[Castle]()
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    val rp = new RandomPlayer(White, conf, explorer) {
      override protected def moveAcceptable(move: Move): Boolean = {
        move match { case a: Castle => castlingMoves = a :: castlingMoves case default => Unit}
        super.moveAcceptable(move)
      }
    }
    val m = rp.getMove
    assertEquals(List(Castle(White, Long)), castlingMoves, "When only long castling was possible short castling was not considered")
  }

  private def longCastlingNotConsiderWhenStartPositionsIncorrect = fail

  private def newRandomPlayer(conf: Configuration): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new RandomPlayer(White, conf, explorer)
  }

  private def newRandomPlayer(conf: Configuration, colour: Colour): Player = {
    val explorer: MoveExplorer = new StandardMoveExplorer(conf)
    new RandomPlayer(colour, conf, explorer)
  }

  private def addKing(conf: Configuration) {
    /* The King is required to allow the kingInCheck method to complete. */
    conf.add("e1", White, King())
  }
}