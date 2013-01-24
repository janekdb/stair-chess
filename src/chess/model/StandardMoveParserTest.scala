package chess.model
import test.TestUtils
import test.Test
import test.Main
import chess.model.Colours.{ Black, White }

// TODO: Rename to StandardMoveParser
object StandardMoveParserTest extends Test with TestUtils with Main {

  implicit def iterableToList(iter: Iterable[Move]): List[Move] = iter.toList

  def runTests {
    invalidMove
    confirmMovePieceParsed
    confirmMovePieceCapturingParsed
    // TODO: Complete StandardMoveParserTest
    // confirmEnPassantParsed
    confirmCastleParsed
    confirmPromoteToKnightParsed
    confirmPromoteToQueenParsed
    confirmPromoteToInvalidPieceParsed
    confirmPromoteCapturingToKnightParsed
    confirmPromoteCapturingToRookParsed
  }

  import StandardMoveParser.parse

  def invalidMove {
    assertEquals(None, parse(getMoves(Black), "h4h5"))
  }

  def confirmMovePieceParsed {
    // TODO: Add test using allMoveTypes
    assertEquals(Some(new MovePiece("e8f8")), parse(getMoves(Black), "e8f8"))
    assertEquals(Some(new MovePiece("e1d2")), parse(getMoves(White), "e1d2"))
  }

  def confirmMovePieceCapturingParsed {
    // TODO: Add test using allMoveTypes
    val conf = getConf
    conf.add(new Position("d7"), White, Pawn())
    val moves = getMoves(conf)
    assertEquals(Some(new MovePieceCapturing("e8d7")), parse(moves, "e8d7"))
  }

  def confirmEnPassantParsed {
    val input = "a7a5"
    val expected: Move = allMoveTypes(input)
    val actual = parse(allMoveTypes.values, input)
    assertEquals(Some(expected), actual)
  }

  def confirmCastleParsed = {
    val input = "castle-short"
    val expected: Move = allMoveTypes(input)
    val actual = parse(allMoveTypes.values, input)
    assertEquals(Some(expected), actual)
  }

  def confirmPromoteToInvalidPieceParsed {
    val input = "e7-plonje"
    val actual = parse(allMoveTypes.values, input)
    assertEquals(None, actual)
  }

  def confirmPromoteToKnightParsed {
    val input = "e7-knight"
    val expected: Move = allMoveTypes(input)
    val Some(actual) = parse(allMoveTypes.values, input)
    assertEquals(expected, actual)
  }

  def confirmPromoteToQueenParsed {
    val input = "e7-queen"
    val expected: Move = allMoveTypes(input)
    val Some(actual) = parse(allMoveTypes.values, input)
    assertEquals(expected, actual)
  }

  def confirmPromoteCapturingToKnightParsed = {
    val input = "e7d8-knight"
    val expected: Move = allMoveTypes(input)
    val Some(actual) = parse(allMoveTypes.values, input)
    assertEquals(expected, actual)
  }

  def confirmPromoteCapturingToRookParsed {
    val input = "e7d8-rook"
    val expected: Move = allMoveTypes(input)
    val Some(actual) = parse(allMoveTypes.values, input)
    assertEquals(expected, actual)
  }

  private def getConf = {
    val conf = new GridConfiguration
    addKings(conf)
    conf
  }

  private def getMoves(colour: Colour): List[Move] = new StandardMoveExplorer(getConf).legalMoves(colour)

  private def getMoves(conf: ConfigurationView) = new StandardMoveExplorer(conf).legalMoves(Black)

  private def allMoveTypes: Map[String, Move] = {
    val s = new Position("e1")
    val e = new Position("e2")
    val e2 = new Position("d2")
    val s3 = new Position("e7")
    val e3 = new Position("d8")
    val s4 = new Position("a7")
    val e4 = new Position("a5")
    // TODO: Determine whether Resign should be moved out of the Move hierarchy */
    /* All except Resign */
    def toString(p1: Position, p2: Position): String = p1.toString + p2.toString
    var m = Map[String, Move]()
    m += (toString(s, e) -> new MovePiece(s, e))
    m += (toString(s, e2) -> new MovePieceCapturing(s, e2))
    m += ("castle-short" -> new Castle(Black, Short))
    m += (s3.toString + "-knight" -> new Promote(s3, Knight()))
    m += (s3.toString + "-queen" -> new Promote(s3, Queen()))
    m += (toString(s3, e3) + "-knight" -> new PromoteCapturing(s3, e3, Knight()))
    m += (toString(s3, e3) + "-rook" -> new PromoteCapturing(s3, e3, Rook()))
    m += (toString(s4, e4) -> EnPassant(s4, e4))
    m
  }
}

