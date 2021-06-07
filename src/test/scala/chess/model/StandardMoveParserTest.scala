package chess.model

import chess.model.Colours.{Black, White}
import org.scalatest._
import matchers.should.Matchers
import chess.test.TestUtils
import wordspec.AnyWordSpec

import scala.util.Random

class StandardMoveParserTest extends AnyWordSpec with Matchers with Inspectors with TestUtils {

  private implicit def iterableToList(iter: Iterable[Move]): List[Move] = iter.toList

  "A StandardMoveParser" should {
    "skip invalid inputs" in {
      invalidInputs()
    }
    "parse valid inputs" in {
      confirmMovePieceParsed()
      confirmMovePieceCapturingParsed()
      confirmEnPassantParsed()
      confirmCastleParsed()
      confirmPromoteToKnightParsed()
      confirmPromoteToQueenParsed()
      confirmPromoteToInvalidPieceParsed()
      confirmPromoteCapturingToKnightParsed()
      confirmPromoteCapturingToRookParsed()
    }
  }

  import StandardMoveParser.parse

  private def invalidInputs(): Assertion = {
    /* Out of bounds */
    parse(getMoves(Black), "h4h5") shouldBe empty
    /* Non-numeric part */
    parse(getMoves(Black), "gggg") shouldBe empty
    /* Monkey test */
    val r = new Random(0)
    forAll(1 to 20) { x =>
      val monkeyText = randomString(r, r.nextInt(10) + 1)
      parse(getMoves(Black), monkeyText) shouldBe empty
    }
  }

  private def randomString(r: Random, n: Int): String = List.fill(n)(r.nextPrintableChar()).mkString

  private def confirmMovePieceParsed(): Assertion = {
    // TODO: Add test using allMoveTypes
    parse(getMoves(Black), "e8f8") should contain(new MovePiece("e8f8"))
    parse(getMoves(White), "e1d2") should contain(new MovePiece("e1d2"))
  }

  private def confirmMovePieceCapturingParsed(): Assertion = {
    // TODO: Add test using allMoveTypes
    val conf = getConf
    conf.add(new Position("d7"), White, Pawn)
    val moves = getMoves(conf)
    parse(moves, "e8d7") should contain(new MovePieceCapturing("e8d7"))
  }

  private def confirmEnPassantParsed(): Assertion = {
    val input          = "a7a5"
    val expected: Move = allMoveTypes(input)
    val actual         = parse(allMoveTypes.values, input)
    actual should contain(expected)
  }

  private def confirmCastleParsed(): Assertion = {
    val input          = "castle-short"
    val expected: Move = allMoveTypes(input)
    val actual         = parse(allMoveTypes.values, input)
    actual should contain(expected)
  }

  private def confirmPromoteToInvalidPieceParsed(): Assertion = {
    val input  = "e7-plonje"
    val actual = parse(allMoveTypes.values, input)
    actual shouldBe empty
  }

  private def confirmPromoteToKnightParsed(): Assertion = {
    val input          = "e7-knight"
    val expected: Move = allMoveTypes(input)
    val actual         = parse(allMoveTypes.values, input)
    actual should contain(expected)
  }

  private def confirmPromoteToQueenParsed(): Assertion = {
    val input          = "e7-queen"
    val expected: Move = allMoveTypes(input)
    val actual         = parse(allMoveTypes.values, input)
    actual should contain(expected)
  }

  private def confirmPromoteCapturingToKnightParsed(): Assertion = {
    val input          = "e7d8-knight"
    val expected: Move = allMoveTypes(input)
    val actual         = parse(allMoveTypes.values, input)
    actual should contain(expected)
  }

  private def confirmPromoteCapturingToRookParsed(): Assertion = {
    val input          = "e7d8-rook"
    val expected: Move = allMoveTypes(input)
    val actual         = parse(allMoveTypes.values, input)
    actual should contain(expected)
  }

  private def getConf = {
    val conf = new GridConfiguration
    addKings(conf)
    conf
  }

  private def getMoves(colour: Colour): List[Move] = new StandardMoveExplorer(getConf).legalMoves(colour)

  private def getMoves(conf: ConfigurationView) = new StandardMoveExplorer(conf).legalMoves(Black)

  private def allMoveTypes: Map[String, Move] = {
    val s  = new Position("e1")
    val e  = new Position("e2")
    val e2 = new Position("d2")
    val s3 = new Position("e7")
    val e3 = new Position("d8")
    val s4 = new Position("a7")
    val e4 = new Position("a5")

    /* All except Resign */
    def toString(p1: Position, p2: Position): String = p1.toString + p2.toString

    var m = Map[String, Move]()
    m += (toString(s, e)               -> MovePiece(s, e))
    m += (toString(s, e2)              -> MovePieceCapturing(s, e2))
    m += ("castle-short"               -> Castle(Black, Short))
    m += (s3.toString + "-knight"      -> Promote(s3, Knight))
    m += (s3.toString + "-queen"       -> Promote(s3, Queen))
    m += (toString(s3, e3) + "-knight" -> PromoteCapturing(s3, e3, Knight))
    m += (toString(s3, e3) + "-rook"   -> PromoteCapturing(s3, e3, Rook))
    m += (toString(s4, e4)             -> EnPassant(s4, e4))
    m
  }
}
