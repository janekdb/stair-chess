package chess.stage

/** Display information related to the staged event which could be a tournament or a 1-1 match.
  */
object Display {

  def renderScoreCard(scoreCard: ScoreCard): Unit = {

    println("Scores:")
    val maxNameWidth = scoreCard.players.map(_.length).max
    val pad          = (s: String) => s.padTo(maxNameWidth, ' ')
    val printScore = (scores: Scores) => {
      val Scores(name, win, draw, lose) = scores
      val total                         = win + draw + lose
      val pctWon                        = 100f * win / total
      println(f"${pad(name)} : $win%4d    $draw%4d    $lose%4d    $total%4d    $pctWon%6.2f")
    }

    scoreCard.getScores.foreach(printScore)

//    println("Wins:")
//
//    val maxNameWidth = scoreCard.players.map(_.length).max
//    val pad = (s: String) => s.padTo(maxNameWidth, ' ')
//    val printScore = (name: String, score: Int) => println(pad(name) + " : " + "%4d" format score)
//
//    scoreCard.getWins.foreach {
//      case (name, score) => printScore(name, score)
//    }
//    println("Draws:")
//    scoreCard.getDraws.foreach {
//      case (name, score) => printScore(name, score)
//    }
  }
}
