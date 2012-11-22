package chess.stage

import scala.collection.mutable.HashMap

import chess.player.Player

class ScoreCard(val players: Set[String]) {

  val wins = new HashMap[String, Int]()
  val draws = new HashMap[String, Int]()

  players.foreach(p => wins.put(p, 0))
  players.foreach(p => draws.put(p, 0))

  def addWin(winner: Player, loser: Player) {
    wins(winner.getName) = wins(winner.getName) + 1
  }

  def addDraw(player1: Player, player2: Player) {
    draws(player1.getName) = draws(player1.getName) + 1
    draws(player2.getName) = draws(player2.getName) + 1
  }

  def getWins: List[(String, Int)] = {
    /* Sort the greatest number of wins first */
    val ordering = new Object with Ordering[(String, Int)] {
      def compare(x: (String, Int), y: (String, Int)) = {
        val (p1, s1) = x
        val (p2, s2) = y
        val sc = -(s1 compare s2)
        /* For tied scores order by name */
        if (sc != 0) sc else p1 compare p2
      }
    }
    wins.toList.sorted(ordering)
  }

  def getDraws: List[(String, Int)] = {
    /* Sort by name because a high draw count is not neccesarily better or worse than a low number */
    val ordering = new Object with Ordering[(String, Int)] {
      def compare(x: (String, Int), y: (String, Int)) = {
        val (p1, _) = x
        val (p2, _) = y
        p1 compare p2
      }
    }
    draws.toList.sorted(ordering)
  }
}
