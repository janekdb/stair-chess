package chess.model

import test.Test

object StandardMoveExplorerTest extends Test {

  // TODO: Find out how to only define this in the superclass  
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests(): Unit = {

    getBasicPositionsIncludesEnPassant
  }

  private def getBasicPositionsIncludesEnPassant = {
    // TODO: Write this test
    val conf = new GridConfiguration
    val e = new StandardMoveExplorer(conf)
    
    val ps = e.getBasicPositions(new Position e5)
    fail
  }
}