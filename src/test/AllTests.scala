package test

object AllTests {

  def tests: List[Test] = {
    var tests: List[Test] = Nil
    // TODO: Determine if each test suite could be added via classpath scanning
    //   which would allow all tests to be looped over to run them.
    import chess.model._
    tests ::= PositionTest
    tests ::= MoveTest
    tests ::= GridConfigurationTest
    tests ::= BoardModelTest
    tests ::= StandardMoveExplorerTest
    import chess.player._
    tests ::= RandomPlayerTest
    tests ::= CapturingPlayerTest
    tests ::= CheckingPlayerTest
    tests ::= ChainedMoveRankerTest
    import chess.app._
    tests ::= ScoreCardTest
    println("AllTests: tests.size: " + tests.size)
    tests
  }

  def runAllTests = tests foreach { _.runTests }
}