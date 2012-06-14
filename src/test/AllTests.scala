package test

object AllTests {

  def tests: List[Test] = {
    var tests: List[Test] = Nil
    // TODO: Determine if each test object could add itself into a object
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
    println("AllTests: tests.size: " + tests.size)
    tests
  }

  def runAllTests = tests foreach { _.runTests }
}