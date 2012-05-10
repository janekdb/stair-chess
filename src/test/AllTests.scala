package test

object AllTests {

  def tests: List[Test] = {
    var tests: List[Test] = Nil
    // TODO: Determine if each test object could add itself into a object
    //   which would allow all tests to be looped over to run them.
    tests = chess.model.PositionTest :: tests
    tests = chess.model.MoveTest :: tests
    tests = chess.model.GridConfigurationTest :: tests
    tests = chess.model.BoardModelTest :: tests
    tests = chess.model.StandardMoveExplorerTest :: tests
    tests = chess.player.RandomPlayerTest :: tests

    println("AllTests: tests.size: " + tests.size)
    tests
  }

  def runAllTests = tests foreach { _.runTests }
}