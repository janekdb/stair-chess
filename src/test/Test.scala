package test

trait Test {

  def runTests: Unit

  def fail: Unit = throw new FailureException("fail")

  def fail(message: String): Unit = throw new FailureException(message)

  def fail(e: Exception): Unit = throw new FailureException(e)

  def unexpected(e: Exception): Unit = throw new FailureException(e, "Unexpected exception:" + e.getClass.getName)

  def assertEquals(expected: Any, actual: Any) {
    assert(expected == actual, "Expected: " + expected + ", actual: " + actual)
  }

  def assertEquals(expected: Any, actual: Any, message: String) {
	  assert(expected == actual, "Expected: " + expected + ", actual: " + actual+": " + message)
  }
  
  def assertNotNull(obj: Any, message: String) {
    assert(obj != null, message)
  }

  class FailureException(e: Exception, message: String) extends RuntimeException(message, e) {

    def this(e: Exception) = this(e, "Unexpected exception: " + e.getClass())

    def this(message: String) = this(null, message)

  }

  def assertExceptionThrown[T <: Exception](assertion: String, expectedException: Class[T])(b: => Unit) {
    var thrown = false
    var correctType = false
    var ex: Any = null
    try {
      b
    } catch {
      case e @ default => {
        thrown = true
        ex = e
        correctType = e.getClass == expectedException
      }
    }
    if (!thrown) {
      fail(assertion)
    } else if (!correctType) {
      fail("Unexpected exception type: Expected: " + expectedException.toString() + ", had: " + ex)
    }
  }

}