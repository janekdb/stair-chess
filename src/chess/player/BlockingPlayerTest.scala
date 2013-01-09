package chess.player
import java.util.concurrent.CountDownLatch
import scala.actors.Actor
import chess.model.GridConfiguration
import chess.model.Move
import test.Main
import test.Test
import test.TestUtils
import chess.model.MovePiece

object BlockingPlayerTest extends Test with TestUtils with Main {

  def runTests {
    getBlocksUntilSetInvoked
    setBlocksUntilGetInvoked
  }

  private def getBlocksUntilSetInvoked {
    val p = new BlockingPlayer("Test")
    val conf = getConf
    val cd1 = new CountDownLatch(1)
    val cd2 = new CountDownLatch(1)
    var moveOpt: Option[Move] = None
    doAsync {
      cd1.countDown
      moveOpt = p.getMove(conf)
      cd2.countDown
    }
    cd1.await
    assert(moveOpt.isEmpty)
    p.setMove("e1e2")
    cd2.await
    assertEquals(Some(new MovePiece("e1e2")), moveOpt)
  }

  private def setBlocksUntilGetInvoked {
    val p = new BlockingPlayer("Test")
    val conf = getConf
    val cd1 = new CountDownLatch(1)
    val cd2 = new CountDownLatch(1)
    var set = false
    doAsync {
      cd1.countDown
      p.setMove("e1e2")
      set = true
      cd2.countDown
    }
    cd1.await
    assertFalse(set)
    val Some(move) = p.getMove(conf)
    cd2.await
    assertEquals(new MovePiece("e1e2"), move)
  }

  // TODO: Move doAsync into TestUtils
  private def doAsync(code: => Unit) {
    class A extends Actor {
      def act = code
    }
    (new A).start
  }

  private def getConf = {
    val conf = new GridConfiguration
    addKings(conf)
    conf
  }
}