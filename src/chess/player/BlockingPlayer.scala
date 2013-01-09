package chess.player
import java.util.concurrent.SynchronousQueue

import chess.model.Configuration
import chess.model.Move

/**
 * This class can be used as a blocking proxy to some source of move selections.
 */
class BlockingPlayer(val name: String) extends Player {

  private val q = new SynchronousQueue[Option[Move]]

  def getMove(configuration: Configuration): Option[Move] = q take

  def getName: String = name

  /**
   * @param Will return immediately if getMove has been called and is still blocked
   * otherwise will block until getMove is called.
   */
  def setMove(move: Move): Unit = q put Some(move)
}