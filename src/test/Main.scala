package test

trait Main {
  self: Test => 
  def main(args: Array[String]) {
    runTests
  }
}