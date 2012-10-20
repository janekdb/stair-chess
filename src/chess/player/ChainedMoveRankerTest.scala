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

object ChainedMoveRankerTest extends Test with TestUtils with Main {

  def runTests {
    rankerCombinationPicksLongestRookMoves
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

    // TODO: Stop inlining discriminator function
    // def discriminator(move: Move): (Int, Int) = (if (isRook(conf, move)) 1 else 0, length(conf)(move))
    val r :: s :: rs = rankedMoves
    verifyDescending(r, s, rs, (move: Move) => (if (isRook(conf, move)) 1 else 0, length(conf)(move)))
  }

  private def rankerCombinationPicksLongestBishopMoves {
    fail("Not written")
  }

  private def rankerCombinationStopsRankingWhenOnlyOneOption {
    fail("Not written")
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
        // TODO: Rework using groupBy
        def iter(moves: List[Move], rookMoves: List[Move], nonRookMoves: List[Move]): List[List[Move]] = {
          if (moves.isEmpty)
            List(rookMoves, nonRookMoves)
          else {
            val m :: ms = moves
            if (isPiece(preferredPiece)(conf, m))
              iter(ms, m :: rookMoves, nonRookMoves)
            else
              iter(ms, rookMoves, m :: nonRookMoves)
          }
        }
        iter(moves, Nil, Nil)
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
    assertTrue(d._1 * 100 + d._2 > e._1 * 100 + e._2, "Adjacent list of moves should be ranked in descending order")
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

  private def isRook = isPiece(Rook())_

  // TODO: Add a length property to SimpleMove or maybe to Move
  private def length(conf: ConfigurationView)(move: Move) = move match {
    case sm: SimpleMove =>
      (sm.start.col - sm.end.col).abs max (sm.start.row - sm.end.row).abs
    case default => 0
  }
}