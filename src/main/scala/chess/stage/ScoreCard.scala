package chess.stage

import chess.player.Player

import scala.collection.mutable

class ScoreCard(val players: Set[String]) {

  val wins = new mutable.HashMap[String, Int]()
  val draws = new mutable.HashMap[String, Int]()
  val scores = new mutable.HashMap[String, Scores]()

  players.foreach(p => wins.put(p, 0))
  players.foreach(p => draws.put(p, 0))
  players.foreach(p => scores.put(p, Scores(p, 0, 0, 0)))

  def addWin(winner: Player, loser: Player): Unit = {
    wins(winner.getName) = wins(winner.getName) + 1
    val winnerScores: Scores = scores(winner.getName)
    scores(winner.getName) = winnerScores.copy(win = winnerScores.win + 1)
    val loserScores: Scores = scores(loser.getName)
    scores(loser.getName) = loserScores.copy(lose = loserScores.lose + 1)
  }

  def addDraw(player1: Player, player2: Player): Unit = {
    draws(player1.getName) = draws(player1.getName) + 1
    draws(player2.getName) = draws(player2.getName) + 1
    val player1Scores = scores(player1.getName)
    scores(player1.getName) = player1Scores.copy(draw = player1Scores.draw + 1)
    val player2Scores = scores(player2.getName)
    scores(player2.getName) = player2Scores.copy(draw = player2Scores.draw + 1)
  }

  def getWins: List[(String, Int)] = {
    /* Sort the greatest number of wins first */
    val ordering = new Object with Ordering[(String, Int)] {
      def compare(x: (String, Int), y: (String, Int)): Int = {
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
      def compare(x: (String, Int), y: (String, Int)): Int = {
        val (p1, _) = x
        val (p2, _) = y
        p1 compare p2
      }
    }
    draws.toList.sorted(ordering)
  }

  def getScores: List[Scores] = {
    scores.toList.sortBy { case (name, _) => name }.map { case (_, scores) => scores }
  }
}

case class Scores(player: String, win: Int, draw: Int, lose: Int) {
  require(player.nonEmpty)
  require(win >= 0)
  require(draw >= 0)
  require(lose >= 0)
}