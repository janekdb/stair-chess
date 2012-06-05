package chess.model

//import Colours.Colour
import chess.util.UnhandledCaseException

trait Configuration {

  /** Add a piece to the configuration with an initial move count of zero */
  def add(position: Position, colour: Colour, piece: Piece): Unit

  /** Throw exception if there is no piece at the given position */
  def remove(position: Position): Unit

  /**
   * Move the piece, incrementing it's move count.
   * Throw exception if there is no piece at the given position.
   * Throw exception if the end position is already occupied.
   */
  def move(start: Position, end: Position)

  /**
   * Return the last move or None
   */
  def getLastMove: Option[(Piece, Position, Position)]

  /** Replace the piece with a the same colour carrying over the move count */
  def replace(position: Position, replacementPiece: Piece)

  def getRows: List[List[(Colour, Piece)]]

  /** Throw exception if there is no piece at the given position. Includes the previous position if any. */
  def getExistingPiece(position: Position): (Colour, Piece, Option[Position])

  /** Include previous position option. */
  def getPiece(position: Position): Option[(Colour, Piece, Option[Position])]

  /** Return positions of all pieces of the given colour and type. */
  def locatePieces(colour: Colour, piece: Piece): List[Position]

  /** Return positions of all pieces of the given colour. */
  def locatePieces(colour: Colour): List[Position]

  /** Return a deep copy of the Configuration */
  def copyOf: Configuration

  /** @return true if a piece exists at the given location */
  def exists(p: Position) = getPiece(p).isDefined

  /** @return true if a piece exists at the given location with the given colour*/
  // TODO: Do this in a monadic style
  def exists(p: Position, c: Colour) = {
    getPiece(p) match {
      case Some((colour, _, _)) => colour == c
      case None => false
    }
  }

  /**
   * Update board configuration. The move must be legal i.e. the caller takes responsibility for
   * ensuring the move is legal.
   * @return A list of UI events to send to listeners.
   */
  def applyMove(move: Move): List[BoardChanged] = {
    move match {
      case MovePiece(start, end) => {
        /* Square unoccupied so just move the piece if not en passant*/
        this.move(start, end)
        List(PieceMoved(start, end))
      }
      case MovePieceCapturing(start, end) => {
        val (colour, piece, _) = this.getExistingPiece(start)
        val (otherColour, _, _) = this.getExistingPiece(end)
        /* The opponents piece is being captured. */
        assert(otherColour != colour)
        this.remove(end)
        this.move(start, end)
        List(PieceMovedCapturing(start, end, end))
      }
      case e: EnPassant => {
        this.move(e.start, e.end)
        this.remove(e.captured)
        List(PieceMovedCapturing(e.start, e.end, e.captured))
      }
      case Castle(colour, castlingType) => {
        val row = colour.homeRow
        val (king, rook) = castlingType.getPositions(row)
        for ((start, end) <- List(king, rook)) {
          this.move(start, end)
        }
        List(Castled(new PieceMoved(king), new PieceMoved(rook)))
      }
      case p @ Promote(start, piece) => {
        val events = applyMove(MovePiece(start, p.end))
        this.replace(p.end, piece)
        Promoted(p.end, piece) :: events reverse
      }
      case PromoteCapturing(start, end, piece) => {
        val events = applyMove(MovePieceCapturing(start, end))
        this.replace(end, piece)
        Promoted(end, piece) :: events reverse
      }
      case Resign(colour) => throw new AssertionError("Resign should not be handled in applyMove")
      case default => throw new UnhandledCaseException(move.toString)
    }
  }

}