package chess.player
import chess.model.GridConfiguration
import chess.model.{ Move, SimpleMove }
import chess.util.TODO
import test.TestUtils
import chess.model.ConfigurationView
import chess.model.Configuration
import test.Test
import test.Main
import chess.model.Colours.{ Black, White }
import chess.model.{ Bishop, Queen, Rook }
import chess.model.StandardMoveExplorer
import chess.model.Piece
import chess.ranker.MoveRanker
import chess.model.MovePiece

object ChainedMoveRankerTest extends Test with TestUtils with Main {

  def runTests {
    rankerCombinationPicksLongestRookMoves
    rankerCombinationPicksLongestBishopMovesSimple
    rankerCombinationExcludesNilLists
    rankerCombinationPicksLongestBishopMoves
    rankerCombinationStopsRankingWhenOnlyOneOption
  }

  /*
    With a configuration where the longest bishop moves are 7 and the longest rook moves are 6
    Add a MoveRanker that prefers rook moves
    Add a MoveRanker that prefers the longest moves
    Confirm the longest rook moves are picked
    */
  private def rankerCombinationPicksLongestRookMoves {
    val rookRanker = newRanker(Rook())
    val moveRanker: MoveRanker = new ChainedMoveRanker(rookRanker, longestRanker)

    val conf = newConf
    val moveExplorer = new StandardMoveExplorer(conf)
    val moves = moveExplorer.legalMoves(White)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    assert(rankedMoves != Nil)
    assertTrue(rankedMoves.length >= 4,
      "Expected at least four lists to allow for the fundamental divisions of " +
        "{rooks, long}, {rooks, short}, {non-rooks, long}, {non-rooks, short}")

    def discriminator: Discriminator = (move: Move) => (if (isRook(conf, move)) 1 else 0, length(conf)(move))
    val r :: s :: rs = rankedMoves
    verifyDescending(r, s, rs, discriminator)
  }

  /*
    With a configuration where the longest bishop moves are 7 and the longest rook moves are 7
    Add a MoveRanker that prefers the longest moves
    Add a MoveRanker that prefers bishop moves
    Confirm the longest bishop moves are picked
    */
  private def rankerCombinationPicksLongestBishopMovesSimple {
    val bishopRanker = newRanker(Bishop())
    val moveRanker: MoveRanker = new ChainedMoveRanker(longestRanker, bishopRanker)

    /* A rook that can move 6 squares */
    val conf: Configuration = new GridConfiguration
    addKings(conf)
    /* A bishop that can move 7 squares */
    conf.add("a1", White, Bishop())
    /* A rook that can move 6 squares */
    conf.add("b1", White, Rook())

    val moveExplorer = new StandardMoveExplorer(conf)
    val moves = moveExplorer.legalMoves(White)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    assert(rankedMoves != Nil)
    assertTrue(rankedMoves.length >= 4,
      "Expected at least four lists to allow for the fundamental divisions of " +
        "{long, bishops}, {long, rooks}, {short, bishops}, {short, rooks}")

    def discriminator: Discriminator = (move: Move) => (length(conf)(move), if (isBishop(conf, move)) 1 else 0)
    val r :: s :: rs = rankedMoves
    verifyDescending(r, s, rs, discriminator)
  }

  private def rankerCombinationExcludesNilLists {
    val bishopRanker = newRanker(Bishop())
    val conf: Configuration = new GridConfiguration
    addKings(conf)
    val moveExplorer = new StandardMoveExplorer(conf)
    val moves = moveExplorer.legalMoves(White)

    /* Confirm the ranker is not flawed. */
    val bishopRanked = bishopRanker.rankMoves(moves, conf)
    assertTrue(bishopRanked.forall(!_.isEmpty), "No empty lists existed in the list of move lists")

    val moveRanker: MoveRanker = new ChainedMoveRanker(longestRanker, bishopRanker)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    assert(rankedMoves != Nil)
    assertTrue(rankedMoves.forall { x => !x.isEmpty }, "There should not be any empty lists: " + rankedMoves)
  }

  /*
    With a configuration where the longest bishop moves are 7 and the longest rook moves are 7
    Add a MoveRanker that prefers the longest moves
    Add a MoveRanker that prefers bishop moves
    Confirm the longest bishop moves are picked
   */
  private def rankerCombinationPicksLongestBishopMoves {
    val bishopRanker = newRanker(Bishop())
    val moveRanker: MoveRanker = new ChainedMoveRanker(longestRanker, bishopRanker)

    val conf = newConf
    /* A rook that can move 6 squares */
    conf.add("d1", White, Rook())

    val moveExplorer = new StandardMoveExplorer(conf)
    val moves = moveExplorer.legalMoves(White)
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, conf)
    assert(rankedMoves != Nil)
    assertTrue(rankedMoves.length >= 4,
      "Expected at least four lists to allow for the fundamental divisions of " +
        "{long, bishops}, {long, rooks}, {short, bishops}, {short, rooks}")

    def discriminator: Discriminator = (move: Move) => (length(conf)(move), if (isBishop(conf, move)) 1 else 0)
    val r :: s :: rs = rankedMoves
    verifyDescending(r, s, rs, discriminator)
  }

  private def rankerCombinationStopsRankingWhenOnlyOneOption {

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

    val moves = List(new MovePiece("e1e2"), new MovePiece("h8h7"), new MovePiece("g8h6"))
    val rankedMoves: List[List[Move]] = moveRanker.rankMoves(moves, new GridConfiguration)
    assertEquals(List(List(new MovePiece("e1e2")), List(new MovePiece("h8h7"), new MovePiece("g8h6"))), rankedMoves)
  }

  private def newConf = {
    val conf: Configuration = new GridConfiguration
    addKings(conf)
    /* A bishop that can move 7 squares */
    conf.add("a1", White, Bishop())
    /* A queen that can move 7 squares */
    conf.add("h1", White, Queen())
    /* A rook that can move 6 squares */
    conf.add("b2", White, Rook())
    /* A rook that can move 5 squares */
    conf.add("c3", White, Rook())
    conf
  }

  private def newRanker(preferredPiece: Piece): MoveRanker =
    new Object with MoveRanker {
      def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
        val (preferred, other) = moves partition (isPiece(preferredPiece)(conf, _))
        List(preferred, other) filterNot (_.isEmpty)
      }
    }

  private val longestRanker = new Object with MoveRanker {
    def rankMoves(moves: List[Move], conf: ConfigurationView): List[List[Move]] = {
      moves.groupBy(length(conf)).toList.sortBy(_._1).map(_._2).reverse
    }
  }

  private def print(rankedMoves: List[List[Move]]) {
    println("rankedMoves:")
    for (moves <- rankedMoves) println(moves)
  }

  private type Discriminator = (Move) => (Int, Int)

  private def assertListNotEmpty(list: List[Any]) = assertTrue(list.size > 0, "List should not be empty")

  /* Test sequence is descending by the ranker criteria */
  def verifyDescending(m: List[Move], n: List[Move], ms: List[List[Move]], descriminator: Discriminator) {
    assertListNotEmpty(m)
    assertListNotEmpty(n)

    /* Verify each move of the same ranking has the same discriminator */
    val d = descriminator(m.head)
    assertTrue(m.tail.forall(descriminator(_) == d), "Equally ranked moves should have the same discrimintor")
    /* n will be checked on the next iteration. */
    val e = descriminator(n.head)
    assertTrue(d._1 * 100 + d._2 >= e._1 * 100 + e._2,
      "Adjacent list of moves should be ranked in descending order: " + m.head + " > " + n.head + ", " + d + " > " + e)
    ms match {
      case Nil => Unit
      case r :: rs => {
        /* Compare next pair */
        verifyDescending(n, r, rs, descriminator)
      }
      case default => fail("Unhandled case: " + ms)
    }
  }

  private def isPiece(targetPiece: Piece)(conf: ConfigurationView, move: Move) = move match {
    case sm: SimpleMove => {
      val (_, piece, _) = conf.getExistingPiece(sm.start)
      targetPiece == piece
    }
    case default => false
  }

  private def isBishop = isPiece(Bishop()) _

  private def isRook = isPiece(Rook()) _

  // TODO: Add a length property to SimpleMove or maybe to Move
  private def length(conf: ConfigurationView)(move: Move) = move match {
    case sm: SimpleMove =>
      (sm.start.col - sm.end.col).abs max (sm.start.row - sm.end.row).abs
    case default => 0
  }
}