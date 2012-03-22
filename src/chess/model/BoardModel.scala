package chess.model

import chess.util.UnhandledCaseException
import chess.util.TODO
import chess.model.ex.{
  //  AttackedPositionException,
  //  CheckedOwnKing,
  //  InterveningPieceException,
  IllegalMoveException
  //  PreviouslyMovedException,
  //  UnreachablePositionException
}
import Misc.kingInCheck
import WinModes.WinMode

/**
 * This class is concerned with maintaining a model of a chess game. It contains no UI. A UI can be attached as a listener.
 *
 * With White at the top of a grid that hangs down the coordinate of a square is (column 1-8, row 1-8).
 */
class BoardModel {

  private val conf: Configuration = new GridConfiguration
  private val moveExplorer: MoveExplorer = new StandardMoveExplorer(conf)
  
  def this(placements: List[(Colour, Piece, Position)], subscribers: List[BoardChangedSubscriber]) {
    this
    subscribers foreach subscribe
    for ((colour, piece, position) <- placements) place(colour, piece, position)
  }

  case class GameOutcome(winner: Colour, winMode: WinMode) {
    def isCheckMate: Boolean = winMode == WinModes.CheckMate
    def isResigned: Boolean = winMode == WinModes.Resignation
  }

  var gameOutcome: GameOutcome = null

  private def wonGuard = if (!isWon) throw new AssertionError("There is no winner")

  def isWon = gameOutcome != null

  def getWinner: Colour = {
    assert(gameOutcome != null)
    gameOutcome.winner
  }

  private def place(colour: Colour, piece: Piece, position: Position){
    assert(colour != null)
    assert(piece != null)
    assert(position != null)
    assert(conf != null)
    conf.add(position, colour, piece)
    subscribers.foreach { _.onBoardChanged(PiecePlaced(colour, piece, position)) }    
  }

  private def setWinState(winMode: WinMode, winner: Colour) {
    this.gameOutcome = GameOutcome(winner, winMode)
  }
    
  def move(move: Move): Unit = {
    debug("Moving: " + move)

    if (gameOutcome != null) {
      throw new IllegalStateException("The game has already been won");
    }
    moveExplorer.rejectIllegalMove(move)
    val events = move match {
      case Resign(colour) => {
        setWinState(WinModes.Resignation, colour.opposite)
        List(Resigned(colour))
      }
      case default => {
        /* Cache off the colour before the move is applied. */
        val colour = extractColour(move)
        val e = conf.applyMove(move)

        // TODO: Move this side-effect out of the case statement
        if (checkForCheckMate(colour.opposite)) {
          setWinState(WinModes.CheckMate, colour)
        }
        e
      } ::: { if (isWon) List(Won(gameOutcome.winner, gameOutcome.winMode)) else Nil }
    }

    for (s <- subscribers; e <- events) { s.onBoardChanged(e) }
  }

  private def extractColour(move: Move): Colour = {
    implicit def tuple2colour(t: (Colour, Piece, Option[Position])) = t._1
    move match {
      case Castle(colour, _) => colour
      case MovePiece(start, _) => conf.getExistingPiece(start)
      case Promote(start, _, _) => conf.getExistingPiece(start)
      case EnPassant(start, _) => conf.getExistingPiece(start)
      case default => throw new UnhandledCaseException(move.toString)
    }
  }

  private def checkForCheckMate(colour: Colour): Boolean = {
    return kingInCheck(colour, conf) && !checkedKingCanEscape(colour, conf)
  }

  /*
   * @return true when there is at least one move that will get the king out of
   * check
   */
  private def checkedKingCanEscape(colour: Colour, conf: Configuration): Boolean = {
    val me = new StandardMoveExplorer(conf)
    def moveIsLegal(move: MovePiece) = {
      try {
        // TODO: Convert from exception throwing to case class return to signal illegal move
        me.rejectIllegalMove(move)
        /* The move did not leave the king in check so there is a way out of check */
        true
      } catch {
        case e: IllegalMoveException => { false };
      }
    }
    conf.locatePieces(colour).exists(start =>
      me.getBasicPositions(start).exists(end => moveIsLegal(MovePiece(start, end))))
  }

  // Debug
  private def debug(s: String): Unit = { println(s) }

  // Events

  var subscribers: List[BoardChangedSubscriber] = Nil

  def subscribe(subscriber: BoardChangedSubscriber) = subscribers ::= subscriber

}

object BoardModel {

  import Colours.{ Black, White }

  def standardPlacements: List[(Colour, Piece, Position)] = {

    val pawns = List.fill(Constants.BOARD_SIZE)(Pawn())
    val others = List(Rook(), Knight(), Bishop(), King(), Queen(), Bishop(), Knight(), Rook())

    def bp(pieces: List[Piece], colour: Colour, row: Int) = {
      pieces.zipWithIndex.map { case (piece, index) => (colour, piece, new Position(index + 1, row)) }
    }
    
    bp(others, White, 1) ::: bp(pawns, White, 2) ::: bp(pawns, Black, 7) ::: bp(others, Black, 8)
  }
}