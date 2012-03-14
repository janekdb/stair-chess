package chess.util

object TODO {

  def log(s: String): Unit = { println(s) }

  def throwRuntimeEx: Nothing = throw new RuntimeException("TODO")

  def throwRuntimeEx(message: String): Nothing = throw new RuntimeException("TODO: " + message)
}