package chess.model.ex

class InvalidStalemateException extends RuntimeException
class EarlyStalemateException extends InvalidStalemateException
// TODO: Rename UnconsideredMovesStalemateException to be less cumbersome
// TODO: Add list of available moves to UnconsideredMovesStalemateException
class UnconsideredMovesStalemateException extends InvalidStalemateException