package chess.model
import test.TestUtils
import test.Test
import test.Main
import chess.model.Colours.{ Black, White }

// TODO: Rename to StandardMoveParser
object StandardMoveParserTest extends Test with TestUtils with Main {

  def runTests {
    invalidMove
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

  def invalidMove {
    assertEquals(None, parse(getMoves(Black), "h4h5"))
  }

  def confirmMovePieceParsed {
    assertEquals(Some(new MovePiece("e8f8")), parse(getMoves(Black), "e8f8"))
    assertEquals(Some(new MovePiece("e1d2")), parse(getMoves(White), "e1d2"))
  }

  def confirmMovePieceCapturingParsed {
    val conf = getConf
    conf.add(new Position("d7"), White, Pawn())
    val moves = getMoves(conf)
    assertEquals(Some(new MovePieceCapturing("e8d7")), parse(moves, "e8d7"))
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

  private def getMoves(colour: Colour): List[Move] = new StandardMoveExplorer(getConf).legalMoves(colour)

  private def getMoves(conf: ConfigurationView) = new StandardMoveExplorer(conf).legalMoves(Black)

}