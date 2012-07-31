package chess.app

import scala.collection.mutable.HashMap

import chess.player.Player

class ScoreCard {
  val wins = new HashMap[String, Int]() { override def default(k: String) = 0 }
  val draws = new HashMap[String, Int]() { override def default(k: String) = 0 }

  def addWin(winner: Player, loser: Player) {
    println("Adding win for " + winner)

    wins(winner.getName) = wins(winner.getName) + 1
    wins(loser.getName) = wins(loser.getName) + 0

    draws(winner.getName) = draws(winner.getName) + 0
    draws(loser.getName) = draws(loser.getName) + 0
  }

  def addDraw(player1: Player, player2: Player) {
    draws(player1.getName) = draws(player1.getName) + 1
    draws(player2.getName) = draws(player2.getName) + 1

    wins(player1.getName) = wins(player1.getName) + 0
    wins(player2.getName) = wins(player2.getName) + 0
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

  def getAbandons: List[(String, Int)] = {
    List()
  }

  override def toString = wins.toString

  // TODO: Remove this method
  // TODO: Display victory scores first ordered by highest number of wins
  // TODO: Move ScoreCard into suitable package
  def displayScores {
    wins.foreach {
      case (k, v) => println(k + ": " + v)
    }
  }
}
