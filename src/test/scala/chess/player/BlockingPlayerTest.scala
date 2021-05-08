package chess.player

import chess.model.Colours.Black
import chess.model._
import test.TestUtils
import org.scalatest._
import wordspec.AnyWordSpec
import matchers.should.Matchers
import OptionValues._
import java.util.concurrent.CountDownLatch
import scala.concurrent.Future

class BlockingPlayerTest extends AnyWordSpec with Matchers with TestUtils {

  "A BlockingPlayer" when {
    "getting a move" should {
      "block get until a move factory is set" in {
        getBlocksUntilSetInvoked()
      }
      "block get until a non-empty move is set" in {
        getBlocksUntilSetInvokedWithSome()
      }
      "block set until get is invoked" in {
        setBlocksUntilGetInvoked()
      }
    }
  }

  private def getBlocksUntilSetInvoked(): Assertion = {
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
    moveOpt.value shouldBe new MovePiece("e1e2")
  }

  private def getBlocksUntilSetInvokedWithSome(): Assertion = {
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
    moveOpt.value shouldBe new MovePiece("e1e2")
  }

  private def setBlocksUntilGetInvoked(): Assertion = {
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
    set shouldBe false
    val moveOpt = p.getMove(conf)
    cd2.await()
    moveOpt.value shouldBe new MovePiece("e1e2")
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

  private def mf(moveOpt: Option[Move]) = new MoveFactory {
    override def getMove(colour: Colour, conf: Configuration): Option[Move] = moveOpt
  }
}
