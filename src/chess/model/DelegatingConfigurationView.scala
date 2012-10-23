package chess.model

import scala.collection.immutable.List

/**
 * Provide an implementation of ConfigurationView by delegating to a Configuration
 */
class DelegatingConfigurationView(val configuration: Configuration) extends ConfigurationView {

  def getLastMove(): Option[(Piece, Position, Position)] = configuration.getLastMove

  def getRows(): List[List[(Colour, Piece)]] = configuration.getRows

  def getExistingPiece(position: Position): (Colour, Piece, Option[Position]) = configuration.getExistingPiece(position)

  def getPiece(position: Position): Option[(Colour, Piece, Option[Position])] = configuration.getPiece(position)

  def locatePieces(colour: Colour, piece: Piece): List[Position] = configuration.locatePieces(colour, piece)

  def locatePieces(colour: Colour): List[Position] = configuration.locatePieces(colour)

  def applied(move: Move): ConfigurationView = {
    val conf = configuration.copyOf
    conf.applyMove(move)
    new DelegatingConfigurationView(conf)
  }

}