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
    getBlocksUntilUseInvoked
    useBlockUntilGetInvoked
  }

  //  private def actorTest {
  //
  //    class Ping extends 
  //      def act() {
  //        .SECONDS.sleep(4)
  //        .println("Hi")
  //      }
  //    }
  //
  //    class Pong extends 
  //      def act() {
  //        .println("Pong")
  //      }
  //    }
  //    val ping = new Ping
  //    ping.start
  //    val pong = new Pong
  //    pong.start
  //    .SECONDS.sleep(10)
  //
  //  }

  private def getBlocksUntilUseInvoked {
    val p = new BlockingPlayer("Test")
    val conf = getConf
    val cd1 = new CountDownLatch(1)
    val cd2 = new CountDownLatch(1)
    var moveOpt: Option[Move] = None
    class Getter extends Actor {
      def act() {
        cd1.countDown
        moveOpt = p.getMove(conf)
        cd2.countDown
      }
    }
    val getter = new Getter
    getter.start
    cd1.await
    assert(moveOpt.isEmpty)
    p.useMove("e1e2")
    cd2.await
    assertEquals(Some(new MovePiece("e1e2")), moveOpt)
  }

  private def useBlockUntilGetInvoked = fail

  private def getConf = {
    val conf = new GridConfiguration
    conf
  }
}