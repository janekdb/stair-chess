package chess.player

import chess.model.Colours.White
import chess.model._
import chess.ranker.MoveRanker
import org.scalatest._
import matchers.should.Matchers
import wordspec.AnyWordSpec
import chess.test.TestUtils

import scala.annotation.tailrec

object ChainedMoveRankerTest extends AnyWordSpec with Matchers with TestUtils {

  "A ChainedMoveRanker" when {
    "selecting a move" should {
      "chain the use of rankers" in {
        rankerCombinationPicksLongestRookMoves()
        rankerCombinationPicksLongestBishopMovesSimple()
        rankerCombinationExcludesNilLists()
        rankerCombinationPicksLongestBishopMoves()
        rankerCombinationStopsRankingWhenOnlyOneOption()
      }
    }
  }

  /*
    With a configuration where the longest bishop moves are 7 and the longest rook moves are 6
    Add a MoveRanker that prefers rook moves
    Add a MoveRanker that prefers the longest moves
    Confirm the longest rook moves are picked
   */
  private def rankerCombinationPicksLongestRookMoves(): Assertion = {
    val rookRanker             = newRanker(Rook)
    val moveRanker: MoveRanker = new ChainedMoveRanker(rookRanker, longestRanker)

    val conf                          = newConf
    val moveExplorer                  = new StandardMoveExplorer(conf)
    val moves                         = moveExplorer.legalMoves(White)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    rankedMoves should not be empty

    withClue(
      "Expected at least four lists to allow for the fundamental divisions of " +
        "{rooks, long}, {rooks, short}, {non-rooks, long}, {non-rooks, short}"
    ) {
      rankedMoves.length should be >= 4
    }

    def discriminator: Discriminator = (move: Move) => (if (isRook(conf, move)) 1 else 0, moveLength(move))

    val r :: s :: rs = rankedMoves
    verifyDescending(r, s, rs, discriminator)
  }

  /*
    With a configuration where the longest bishop moves are 7 and the longest rook moves are 7
    Add a MoveRanker that prefers the longest moves
    Add a MoveRanker that prefers bishop moves
    Confirm the longest bishop moves are picked
   */
  private def rankerCombinationPicksLongestBishopMovesSimple(): Assertion = {
    val bishopRanker           = newRanker(Bishop)
    val moveRanker: MoveRanker = new ChainedMoveRanker(longestRanker, bishopRanker)

    /* A rook that can move 6 squares */
    val conf: Configuration = new GridConfiguration
    addKings(conf)
    /* A bishop that can move 7 squares */
    conf.add("a1", White, Bishop)
    /* A rook that can move 6 squares */
    conf.add("b1", White, Rook)

    val moveExplorer                  = new StandardMoveExplorer(conf)
    val moves                         = moveExplorer.legalMoves(White)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    rankedMoves should not be empty
    withClue(
      "Expected at least four lists to allow for the fundamental divisions of " +
        "{long, bishops}, {long, rooks}, {short, bishops}, {short, rooks}"
    ) {
      rankedMoves.length should be >= 4
    }

    def discriminator: Discriminator = (move: Move) => (moveLength(move), if (isBishop(conf, move)) 1 else 0)

    val r :: s :: rs = rankedMoves
    verifyDescending(r, s, rs, discriminator)
  }

  private def rankerCombinationExcludesNilLists(): Assertion = {
    val bishopRanker        = newRanker(Bishop)
    val conf: Configuration = new GridConfiguration
    addKings(conf)
    val moveExplorer = new StandardMoveExplorer(conf)
    val moves        = moveExplorer.legalMoves(White)

    /* Confirm the ranker is not flawed. */
    val bishopRanked = bishopRanker.rankMoves(moves, conf)
    withClue("No empty lists existed in the list of move lists") {
      all(bishopRanked) should not be empty
    }

    val moveRanker: MoveRanker        = new ChainedMoveRanker(longestRanker, bishopRanker)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    rankedMoves should not be empty
    withClue("There should not be any empty lists: " + rankedMoves) {
      all(rankedMoves) should not be empty
    }
  }

  /*
    With a configuration where the longest bishop moves are 7 and the longest rook moves are 7
    Add a MoveRanker that prefers the longest moves
    Add a MoveRanker that prefers bishop moves
    Confirm the longest bishop moves are picked
   */
  private def rankerCombinationPicksLongestBishopMoves(): Assertion = {
    val bishopRanker           = newRanker(Bishop)
    val moveRanker: MoveRanker = new ChainedMoveRanker(longestRanker, bishopRanker)

    val conf = newConf
    /* A rook that can move 6 squares */
    conf.add("d1", White, Rook)

    val moveExplorer                  = new StandardMoveExplorer(conf)
    val moves                         = moveExplorer.legalMoves(White)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    rankedMoves should not be empty
    withClue(
      "Expected at least four lists to allow for the fundamental divisions of " +
        "{long, bishops}, {long, rooks}, {short, bishops}, {short, rooks}"
    ) {
      rankedMoves.length should be >= 4
    }

    def discriminator: Discriminator = (move: Move) => (moveLength(move), if (isBishop(conf, move)) 1 else 0)

    val r :: s :: rs = rankedMoves
    verifyDescending(r, s, rs, discriminator)
  }

  private def rankerCombinationStopsRankingWhenOnlyOneOption(): Assertion = {

    val headRanker = new Object with MoveRanker {
      def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
        List(List(moves.head), moves.tail)
      }
    }
    val assertionMessage = "throwingRanker should not have been invoked for a singleton move list"
    val throwingRanker = new Object with MoveRanker {
      def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
        if (moves.size == 1)
          throw new AssertionError(assertionMessage)
        List(moves.sortBy { case MovePiece(p1, p2) => p1.col > p2.col })
      }
    }

    val moveRanker: MoveRanker = new ChainedMoveRanker(headRanker, throwingRanker)

    val moves                         = List(new MovePiece("e1e2"), new MovePiece("h8h7"), new MovePiece("g8h6"))
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, new GridConfiguration)
    rankedMoves shouldBe List(List(new MovePiece("e1e2")), List(new MovePiece("h8h7"), new MovePiece("g8h6")))
  }

  private def newConf = {
    val conf: Configuration = new GridConfiguration
    addKings(conf)
    /* A bishop that can move 7 squares */
    conf.add("a1", White, Bishop)
    /* A queen that can move 7 squares */
    conf.add("h1", White, Queen)
    /* A rook that can move 6 squares */
    conf.add("b2", White, Rook)
    /* A rook that can move 5 squares */
    conf.add("c3", White, Rook)
    conf
  }

  private def newRanker(preferredPiece: Piece): MoveRanker =
    (moves: List[Move], conf: ConfigurationView) => {
      val (preferred, other) = moves partition (isPiece(preferredPiece)(conf, _))
      List(preferred, other) filterNot (_.isEmpty)
    }

  private val longestRanker = new Object with MoveRanker {
    def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
      val grouped = moves.groupBy(moveLength).toList
      grouped.sortBy(_._1).map(_._2).reverse
    }
  }

  private type Discriminator = Move => (Int, Int)

  /* Test sequence is descending by the ranker criteria */
  @tailrec
  def verifyDescending(m: List[Move], n: List[Move], ms: List[List[Move]], discriminator: Discriminator): Assertion = {
    m should not be empty
    n should not be empty

    /* Verify each move of the same ranking has the same discriminator */
    val d = discriminator(m.head)
    withClue("Equally ranked moves should have the same discriminator") {
      all(m.tail.map(discriminator)) shouldBe d
    }
    /* n will be checked on the next iteration. */
    val e = discriminator(n.head)
    withClue(
      "Adjacent list of moves should be ranked in descending order: " + m.head + " > " + n.head + ", " + d + " > " + e
    ) {
      (d._1 * 100 + d._2) shouldBe >=(e._1 * 100 + e._2)
    }
    ms match {
      case Nil     => succeed
      case r :: rs =>
        /* Compare next pair */
        verifyDescending(n, r, rs, discriminator)
      case _ => fail("Unhandled case: " + ms)
    }
  }

  private def isPiece(targetPiece: Piece)(conf: ConfigurationView, move: Move) = move match {
    case sm: SimpleMove =>
      val piece= conf.getExistingPiece(sm.start).piece
      targetPiece == piece
    case _ => false
  }

  private def isBishop = isPiece(Bishop) _

  private def isRook = isPiece(Rook) _

  // TODO: Add a length property to SimpleMove or maybe to Move
  private def moveLength(move: Move) = move match {
    case sm: SimpleMove =>
      (sm.start.col - sm.end.col).abs max (sm.start.row - sm.end.row).abs
    case _ => 0
  }
}
