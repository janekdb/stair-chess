package test

object AllTests {

  def tests: List[Test] = {
    var tests: List[Test] = Nil

    implicit class TestOps(any: Test) { def ++ : Unit = { tests ::= any } }

    import chess.model._
    PositionTest.++
    MoveTest.++
    GridConfigurationTest.++
    BoardModelTest.++
    StandardMoveExplorerTest.++
    DelegatingConfigurationViewTest.++
    StandardMoveParserTest.++

    import chess.player._
    ChainedMoveRankerTest.++
    BlockingPlayerTest.++

    import chess.stage._
    ScoreCardTest.++

    println("AllTests: tests.size: " + tests.size)
    tests
  }

  def runAllTests: Unit = tests foreach { _.runTests }
}