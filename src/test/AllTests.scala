package test

object AllTests {

  def tests: List[Test] = {
    var tests: List[Test] = Nil
    // TODO: Determine if each test suite could be added via classpath scanning
    //   which would allow all tests to be looped over to run them.
    implicit def addTest(any: Test) = new { def ++ { tests ::= any } }

    import chess.model._
    PositionTest.++
    MoveTest.++
    GridConfigurationTest.++
    BoardModelTest.++
    StandardMoveExplorerTest.++
    DelegatingConfigurationViewTest.++

    import chess.player._
    RandomPlayerTest.++
    ChainedMoveRankerTest.++
    BlockingPlayerTest.++

    import chess.ranker._
    CheckingRankerTest.++
    CheckMatingRankerTest.++
    CapturingRankerTest.++
    CaptureEvadingRankerTest.++

    import chess.stage._
    ScoreCardTest.++

    println("AllTests: tests.size: " + tests.size)
    tests
  }

  def runAllTests = tests foreach { _.runTests }
}