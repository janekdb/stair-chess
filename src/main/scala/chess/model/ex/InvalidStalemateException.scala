package chess.model.ex
import chess.model.Move

class InvalidStalemateException extends RuntimeException
class EarlyStalemateException   extends InvalidStalemateException
// TODO: Rename UnconsideredMovesStalemateException to be less cumbersome
class UnconsideredMovesStalemateException(moves: List[Move]) extends InvalidStalemateException {
  override def toString: String = {
    "Invalid stalemate, legal moves available: " + moves
  }
}
