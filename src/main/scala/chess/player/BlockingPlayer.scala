package chess.player
import java.util.concurrent.SynchronousQueue
import chess.model.Configuration
import chess.model.Move
import chess.model.Colour
import chess.model.MoveFactory
import scala.language.postfixOps

/** This class can be used as a blocking proxy to some source of move selections.
  */
class BlockingPlayer(val colour: Colour, val name: String) extends Player {

  private val q = new SynchronousQueue[MoveFactory]

  def getMove(configuration: Configuration): Option[Move] = Some(getSomeMove(configuration))

  /** Do not return until a move has been parsed
    */
  private def getSomeMove(configuration: Configuration): Move = {
    val m = (q take) getMove (colour, configuration)
    m.getOrElse(getSomeMove(configuration))
  }

  def getName: String = name

  def getColour: Colour = colour

  /** Will return immediately if getMove has been called and is still blocked otherwise will block until getMove is
    * called.
    */
  def setMoveFactory(moveFactory: MoveFactory): Unit = q put moveFactory
}
