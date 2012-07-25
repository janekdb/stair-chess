package chess.app

import scala.collection.mutable.HashMap
import chess.player.Player

// TODO: Add ScoreCardTest
class ScoreCard {
  val scores = new HashMap[String, Int]() { override def default(k: String) = 0 }

  def addWin(winner: Player, loser: Player) {
    println("Adding win for " + winner)
    val record = winner.getName + " beat " + loser.getName

    scores(record) = scores(record) + 1
  }

  def addDraw(player1: Player, player2: Player){
    // TODO: Record draw
  }
  
  def addAbandon(player1: Player, player2: Player){
    // TODO: Record abandon
  }
  
  override def toString = scores.toString

  // TODO: Display victory scores first ordered by highest number of wins
  // TODO: Move ScoreCard into suitable package
  def displayScores {
    scores.foreach {
      case (k, v) => println(k + ": " + v)
    }
  }
}
