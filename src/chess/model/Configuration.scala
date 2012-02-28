package chess.model

//import Colours.Colour
import chess.util.UnhandledCaseException

trait Configuration {

  /** Add a piece to the configuration with an initial move count of zero */
  def add(position: Position, colour: Colour, piece: Piece): Unit

  /** Throw exception if there is no piece at the given position */
  def remove(position: Position): Unit

  // TODO: Determine if Configuration.move can be non-public
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

  /**
   * Update board configuration. The move must be legal i.e. the caller takes responsibility for
   * ensuring the move is legal.
   * @return A list of UI events to send to listeners.
   */
  def applyMove(move: Move): List[BoardChanged] = {
    move match {
      case MovePiece(start, end) => {
        val (colour, piece, _) = this.getExistingPiece(start)
        this.getPiece(end) match {
          case None => {
            /* Square unoccupied so just move the piece if not en passant*/
            this.move(start, end)
            List(PieceMoved(start, end))
          }
          case Some((otherColour, _, _)) =>
            {
              /* The opponents piece is being captured. */
              assert(otherColour != colour)
              this.remove(end)
              this.move(start, end)
            }
            // TODO: Handle en-passant
            List(PieceMovedTaking(start, end, end))
        }
      }
      case EnPassant(start, end) => {
        this.move(start, end)
        // TODO: Add method to EnPassant to calculate taken piece position
        val taken = new Position(end.getCol, start.getRow)
        this.remove(taken)
        // TODO: Apply EnPassant resulting in PieceMovedTaking
        List(PieceMovedTaking(start, end, end))
      }
      case Castle(colour, castlingType) => {
        val row = colour.homeRow
        val (king, rook) = castlingType.getPositions(row)
        for ((start, end) <- List(king, rook)) {
          this.move(start, end)
        }
        List(Castled(new PieceMoved(king), new PieceMoved(rook)))
      }
      case Promote(start, end, piece) => {
        Promoted(end, piece) :: applyMove(MovePiece(start, end)) reverse
      }
      case Resign(colour) => throw new AssertionError("Resign should not be handled in applyMove")
      case default => throw new UnhandledCaseException(move.toString)
    }
  }

}