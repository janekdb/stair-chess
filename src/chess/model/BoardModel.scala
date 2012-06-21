package chess.model

import chess.model.ex.EarlyStalemateException
import chess.model.ex.IllegalMoveException
import chess.model.ex.InvalidStalemateException
import chess.model.ex.UnconsideredMovesStalemateException
import chess.util.UnhandledCaseException
import GameOutcomeModes.GameOutcomeMode

// TODO: End game when the last n positions have been repeated checking for the value of n
//   when only two kings
//   when stalemate
//		  abcdefgh
//		8 ��������
//		7 ��������
//		6 ��������
//		5 ��������
//		4 ������n�
//		3 ��������
//		2 ��������
//		1 �����k�K
//		  abcdefgh


/**
 * This class is concerned with maintaining a model of a chess game. It contains no UI. A UI can be attached as a listener.
 *
 * With White at the top of a grid that hangs down the coordinate of a square is (column 1-8, row 1-8).
 */
class BoardModel {

  private val conf: Configuration = new GridConfiguration
  private val moveExplorer: MoveExplorer = new StandardMoveExplorer(conf)

  def getMoveExplorer = moveExplorer
  
  def this(placements: List[(Colour, Piece, Position)], subscribers: List[BoardChangedSubscriber], confChangedSubscribers: List[ConfigurationChangedSubscriber]) {
    this
    subscribers foreach subscribe
    val confView = new DelegatingConfigurationView(conf)
    confChangedSubscribers foreach { _.onConfigurationChanged(confView) }
    for ((colour, piece, position) <- placements) place(colour, piece, position)
  }

  // TODO: Rename to gameOutcomeMode
  case class GameOutcome(winMode: GameOutcomeMode, winner: Option[Colour]) {
    def isCheckMate: Boolean = winMode == GameOutcomeModes.CheckMate
    def isResigned: Boolean = winMode == GameOutcomeModes.Resignation
    def isStalemate: Boolean = winMode == GameOutcomeModes.Stalemate
  }

  var gameOutcome: Option[GameOutcome] = None

  private def wonGuard = if (!isWon) throw new AssertionError("There is no winner")

  def isWon = gameOutcome.isDefined && (gameOutcome.get.isCheckMate || gameOutcome.get.isResigned)

  def isCompleted = gameOutcome.isDefined

  def isDrawn = gameOutcome.isDefined && (gameOutcome.get.isStalemate)

  def getGameOutcome = gameOutcome

  def getWinner: Option[Colour] = {
    gameOutcome.get.winner
  }

  private def place(colour: Colour, piece: Piece, position: Position){
    assert(colour != null)
    assert(piece != null)
    assert(position != null)
    assert(conf != null)
    conf.add(position, colour, piece)
    subscribers.foreach { _.onBoardChanged(PiecePlaced(colour, piece, position)) }    
  }

  private def setGameOutcome(gameOutcomeMode: GameOutcomeMode, winnerOpt: Option[Colour]) {
    this.gameOutcome = Some(GameOutcome(gameOutcomeMode, winnerOpt))
  }

  private var lastColour: Option[Colour] = None

  def move(optMove: Option[Move]): Unit = {

    if (gameOutcome.isDefined) {
      throw new IllegalStateException("The game has already been completed");
    }

    if (optMove.isDefined) {
      moveExplorer.rejectIllegalMove(optMove.get)
    } else {
      if (lastColour.isEmpty) throw new EarlyStalemateException
      val moves = moveExplorer.legalMoves(lastColour.get.opposite)
      if (moves.nonEmpty) throw new UnconsideredMovesStalemateException
    }

    /* Extract the last colour before the configuration is changed. */
    val optColour = optMove match {
      case Some(Resign(colour)) => Some(colour)
      case None => None
      case default => Some(extractColour(optMove.get))
    }

    val (events: List[BoardChanged], outcomeOpt) = optMove match {
      case Some(Resign(colour)) => {
        // TODO: Remove this redundant setWinState call
        setGameOutcome(GameOutcomeModes.Resignation, Some(colour.opposite))
        (List(Resigned(colour)), Some(GameOutcome(GameOutcomeModes.Resignation, Some(colour.opposite))))
      }
      case None => {
        // TODO: Remove this redundant setWinState call
        setGameOutcome(GameOutcomeModes.Stalemate, None)
        (List(), Some(GameOutcome(GameOutcomeModes.Stalemate, None)))
      }
      case default => {
        val move = optMove.get
        val colour = optColour.get
        val e = conf.applyMove(move)
        val outcomeOption = if (checkForCheckMate(colour.opposite)) Some(GameOutcome(GameOutcomeModes.CheckMate, Some(colour))) else None
        (e, outcomeOption)
      }
    }
    if (outcomeOpt.isDefined) {
      val g = outcomeOpt.get
      setGameOutcome(g.winMode, g.winner)
    }
    val wonEvent = if (isWon) List(Won(gameOutcome.get.winner.get, gameOutcome.get.winMode)) else Nil
    val drawnEvent = if (isDrawn) List(Drawn(GameOutcomeModes.Stalemate)) else Nil
    for (s <- subscribers; e <- events ::: wonEvent ::: drawnEvent) { s.onBoardChanged(e) }

    lastColour = optColour
  }

  private def extractColour(move: Move): Colour = {
    implicit def tuple2colour(t: (Colour, Piece, Option[Position])) = t._1
    move match {
      case Castle(colour, _) => colour
      case MovePiece(start, _) => conf.getExistingPiece(start)
      case MovePieceCapturing(start, _) => conf.getExistingPiece(start)
      case Promote(start, _) => conf.getExistingPiece(start)
      case PromoteCapturing(start, _, _) => conf.getExistingPiece(start)
      case EnPassant(start, _) => conf.getExistingPiece(start)
      case default => throw new UnhandledCaseException(move.toString)
    }
  }

  private def checkForCheckMate(colour: Colour): Boolean = {
    moveExplorer.kingInCheck(colour) && !checkedKingCanEscape(colour, conf)
  }

  /*
   * @return true when there is at least one move that will get the king out of
   * check
   */
  private def checkedKingCanEscape(colour: Colour, conf: Configuration): Boolean = {
    val me = new StandardMoveExplorer(conf)
    me.legalMoves(colour).nonEmpty
  }

  // Debug
  private def debug(s: String): Unit = { println(s) }

  // Events

  private var subscribers: List[BoardChangedSubscriber] = Nil

  def subscribe(subscriber: BoardChangedSubscriber) { subscribers ::= subscriber }

}

object BoardModel {

  import Colours.{ Black, White }

  def standardPlacements: List[(Colour, Piece, Position)] = {

    val pawns = List.fill(Constants.BOARD_SIZE)(Pawn())
    val others = List(Rook(), Knight(), Bishop(), Queen(), King(), Bishop(), Knight(), Rook())

    def bp(pieces: List[Piece], colour: Colour, row: Int) = {
      pieces.zipWithIndex.map { case (piece, index) => (colour, piece, new Position(index + 1, row)) }
    }
    
    bp(others, White, 1) ::: bp(pawns, White, 2) ::: bp(pawns, Black, 7) ::: bp(others, Black, 8)
  }
}