package chess.model.ex

import chess.model.MovePiece
import chess.model.Move
import chess.model.Position


// TODO: Make this exception abstract
abstract class IllegalMoveException(move: Move) extends RuntimeException("Illegal move: " + move.toString())

//// TODO: Use better name for MoveOntoOwnPieceException
//class OntoOwnPieceException(move: Move) extends IllegalMoveException(move)

class PreviouslyMovedException(move: Move) extends IllegalMoveException(move)

class InterveningPieceException(move: Move, interveningPiece: Position) extends IllegalMoveException(move) {
  override def toString(): String = super.toString + ", intervening piece at " + interveningPiece
}

class AttackedPositionException(move: Move, attackedPosition: Position) extends IllegalMoveException(move) {
  override def toString(): String = super.toString + ", attacked position " + attackedPosition
}

class UnreachablePositionException(move: Move, legalPositions: List[Position]) extends IllegalMoveException(move) {
  override def toString(): String = "Position was unreachable: " + move + ", legal positions: " + legalPositions
}

class CheckedOwnKing(move: Move) extends IllegalMoveException(move) {
  override def toString(): String = "The move left the players own King in check: " + move
}