package chess.player
import java.util.concurrent.CountDownLatch
import chess.model.GridConfiguration
import chess.model.Move
import test.Main
import test.Test
import test.TestUtils
import chess.model.MovePiece
import chess.model.Colours.Black
import chess.model.MoveFactory
import chess.model.Colour
import chess.model.Configuration

import scala.concurrent.Future

object BlockingPlayerTest extends Test with TestUtils with Main {

  def runTests: Unit = {
    getBlocksUntilSetInvoked
    getBlocksUntilSetInvokedWithSome
    setBlocksUntilGetInvoked
  }

  private def getBlocksUntilSetInvoked(): Unit = {
    val p = new BlockingPlayer(Black, "Test")
    val conf = getConf
    val cd1 = new CountDownLatch(1)
    val cd2 = new CountDownLatch(1)
    var moveOpt: Option[Move] = None
    doAsync {
      cd1.countDown()
      moveOpt = p.getMove(conf)
      cd2.countDown()
    }
    cd1.await()
    assert(moveOpt.isEmpty)
    p.setMoveFactory(mf("e1e2"))
    cd2.await()
    assertEquals(Some(new MovePiece("e1e2")), moveOpt)
  }

  private def getBlocksUntilSetInvokedWithSome(): Unit = {
    val p = new BlockingPlayer(Black, "Test")
    val conf = getConf
    val cd1 = new CountDownLatch(1)
    val cd2 = new CountDownLatch(1)
    var moveOpt: Option[Move] = None
    doAsync {
      cd1.countDown()
      moveOpt = p.getMove(conf)
      cd2.countDown()
    }
    cd1.await()
    assert(moveOpt.isEmpty)
    /* Invoke setMoveFactory multiple times to prove None is discarded. */
    p.setMoveFactory(mf(None))
    p.setMoveFactory(mf(None))
    p.setMoveFactory(mf(None))
    p.setMoveFactory(mf("e1e2"))
    cd2.await()
    assertEquals(Some(new MovePiece("e1e2")), moveOpt)
  }

  private def setBlocksUntilGetInvoked(): Unit = {
    val p = new BlockingPlayer(Black, "Test")
    val conf = getConf
    val cd1 = new CountDownLatch(1)
    val cd2 = new CountDownLatch(1)
    var set = false
    doAsync {
      cd1.countDown()
      p.setMoveFactory(mf("e1e2"))
      set = true
      cd2.countDown()
    }
    cd1.await()
    assertFalse(set)
    val Some(move) = p.getMove(conf)
    cd2.await()
    assertEquals(new MovePiece("e1e2"), move)
  }

  // TODO: Move doAsync into TestUtils
  private def doAsync(code: => Unit): Unit = {
    // the following is equivalent to `implicit val ec = ExecutionContext.global`
    import scala.concurrent.ExecutionContext.Implicits.global
    Future(code)
  }

  private def getConf = {
    val conf = new GridConfiguration
    addKings(conf)
    conf
  }

  private def mf(moveOpt: Option[Move]) = new MoveFactory { override def getMove(colour: Colour, conf: Configuration): Option[Move] = moveOpt }
}