package chess.model
import test.TestUtils
import test.Test
import test.Main
import chess.model.Colours.{ Black, White }

// TODO: Rename to StandardMoveParser
object StandardMoveParserTest extends Test with TestUtils with Main {

  def runTests {
    confirmMovePieceParsed
    confirmMovePieceCapturingParsed
    // TODO: Complete MoveParserTest
    //    confirmResignParsed
    //    confirmCastleParsed
    //    confirmPromoteParsed
    //    confirmPromoteCapturingParsed
    //    confirmEnPassantParsed
  }

  import StandardMoveParser.parse

  def confirmMovePieceParsed {
    val conf = getConf
    assertEquals(Some(new MovePiece("e8f8")), parse(Black, conf, "e8f8"))
    assertEquals(Some(new MovePiece("e1d2")), parse(White, conf, "e1d2"))
  }

  def confirmMovePieceCapturingParsed {
    val conf = getConf
    conf.add(new Position("d7"), White, Pawn())
    assertEquals(Some(new MovePieceCapturing("e8d7")), parse(Black, conf, "e8d7"))
  }

  def confirmResignParsed = fail
  def confirmCastleParsed = fail
  def confirmPromoteParsed = fail
  def confirmPromoteCapturingParsed = fail
  def confirmEnPassantParsed = fail

  private def getConf = {
    val conf = new GridConfiguration
    addKings(conf)
    conf
  }
}