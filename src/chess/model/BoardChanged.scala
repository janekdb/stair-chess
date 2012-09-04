package chess.model

import GameOutcomeModes.GameOutcomeMode

abstract class BoardChanged

/* Assume the consumer of BoardChangeEvent has access to the board configuration. */
case class PiecePlaced(colour: Colour, piece: Piece, position: Position) extends BoardChanged
case class PieceMoved(start: Position, end: Position) extends BoardChanged {
  def this(move: (Position, Position)) = this(move._1, move._2)
}
/* En-passant allows the position of the taken position to be distinct from the end position of the taking piece. */
case class PieceMovedCapturing(start: Position, end: Position, captured: Position) extends BoardChanged
case class Resigned(colour: Colour) extends BoardChanged
case class Castled(king: PieceMoved, rook: PieceMoved) extends BoardChanged
case class Promoted(pawn: Position, replacement: Piece) extends BoardChanged
case class Won(colour: Colour, winMode: GameOutcomeMode) extends BoardChanged
case class Drawn(drawMode: GameOutcomeMode) extends BoardChanged