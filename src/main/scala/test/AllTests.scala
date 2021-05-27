package test

object AllTests {

  def tests: List[Test] = {
    var tests: List[Test] = Nil

    implicit class TestOps(any: Test) { def ++ : Unit = { tests ::= any } }

    import chess.model._

    println("AllTests: tests.size: " + tests.size)
    tests
  }

  def runAllTests: Unit = tests foreach { _.runTests }
}