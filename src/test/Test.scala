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
    assert(expected == actual, "Expected: " + expected + ", actual: " + actual + ": " + message)
  }

  def assertNotEquals(notExpected: Any, actual: Any) {
    assert(notExpected != actual, "Not expected: " + notExpected + ", actual: " + actual)
  }

  def assertNotEquals(notExpected: Any, actual: Any, message: String) {
    assert(notExpected != actual, "Not expected: " + notExpected + ", actual: " + actual + ": " + message)
  }

  def assertNotNull(obj: Any, message: String) {
    assert(obj != null, message)
  }

  def assertTrue(condition: Boolean, message: String) {
    assert(condition, message)
  }

  def assertFalse(condition: Boolean, message: String) {
	  assert(!condition, message)
  }

  class FailureException(e: Exception, message: String) extends RuntimeException(message, e) {

    def this(e: Exception) = this(e, "Unexpected exception: " + e.getClass())

    def this(message: String) = this(null, message)

  }

  def assertIsInstanceOf[T <: Any](expectedType: Class[T], actual: Any, message: String) {
    assertNotNull(actual, message)
    assertEquals(expectedType, actual.getClass, message)
  }

  def assertExceptionThrown[T <: Exception](assertion: String, expectedException: Class[T])(b: => Unit) {
    var thrown = false
    var correctType = false
    var ex: Exception = null
    try {
      b
    } catch {
      case e: Exception => {
        thrown = true
        ex = e
        correctType = e.getClass == expectedException
      }
      case d @ default => throw new AssertionError("Unhandled case: " + d)
    }
    if (!thrown) {
      fail(assertion)
    } else if (!correctType) {
      /* Rethrowing provides more information than using fail. */
      throw ex
    }
  }

}