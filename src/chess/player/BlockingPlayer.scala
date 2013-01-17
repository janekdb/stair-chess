package chess.player
import java.util.concurrent.SynchronousQueue
import chess.model.Configuration
import chess.model.Move
import chess.model.Colour
import chess.model.MoveFactory

/**
 * This class can be used as a blocking proxy to some source of move selections.
 */
class BlockingPlayer(val colour: Colour, val name: String) extends Player {

  private val q = new SynchronousQueue[MoveFactory]

  def getMove(configuration: Configuration): Option[Move] = (q take) getMove (colour, configuration)

  def getName: String = name

  def getColour = colour

  /**
   * @param Will return immediately if getMove has been called and is still blocked
   * otherwise will block until getMove is called.
   */
  def setMoveFactory(moveFactory: MoveFactory): Unit = q put moveFactory
}