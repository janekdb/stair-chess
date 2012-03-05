package test

trait Test {

  def runTests: Unit

  def fail: Unit = throw new FailureException("fail")

  def fail(message: String): Unit = throw new FailureException(message)

  def fail(e: Exception): Unit = throw new FailureException(e)

  def unexpected(e: Exception): Unit = throw new FailureException(e, "Unexpected exception:" + e.getClass.getName)

  def assertEquals(expected: Any, actual: Any) = {
    assert(expected == actual, "Expected: " + expected + ", actual: " + actual)
  }

  def assertNotNull(obj: Any, message: String) = {
    assert(obj != null, message)
  }

  class FailureException(e: Exception, message: String) extends RuntimeException(message, e) {

    // TODO: Add the name of the unexpected exception to the message.
    def this(e: Exception) = this(e, "Unexpected exception" + e.getClass())

    def this(message: String) = this(null, message)

  }

}