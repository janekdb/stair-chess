package chess.model.ex

import chess.model.MovePiece
import chess.model.Move
import chess.model.Position
import chess.model.Colour
import chess.model.Piece

abstract class IllegalMoveException(move: Move) extends RuntimeException("Illegal move: " + move.toString)

class PreviouslyMovedException(move: Move) extends IllegalMoveException(move)

class InterveningPieceException(move: Move, interveningPiece: Position) extends IllegalMoveException(move) {
  override def toString(): String = super.toString + ", intervening piece at " + interveningPiece
}

class AttackedPositionException(move: Move, attackedPosition: Position) extends IllegalMoveException(move) {
  override def toString(): String = super.toString + ", attacked position " + attackedPosition
}

class UnreachablePositionException(move: Move, legalPositions: Set[Position]) extends IllegalMoveException(move) {
  override def toString(): String = "Position was unreachable: " + move + ", legal positions: " + legalPositions
}

class InvalidParticipantException(move: Move, participant: Piece) extends IllegalMoveException(move) {
  override def toString(): String = "Invalid participant for move: " + move + ", participant: " + participant
}

class CheckedOwnKing(move: Move) extends IllegalMoveException(move) {
  override def toString(): String = "The move left the players own King in check: " + move
}

class NonPromotingPawnAdvance(move: Move) extends IllegalMoveException(move) {
  override def toString(): String = "Pawn advance required a promotion: " + move
}

class NonCapturingMoveException(move: Move) extends IllegalMoveException(move)

class CapturingMoveException(move: Move) extends IllegalMoveException(move)
