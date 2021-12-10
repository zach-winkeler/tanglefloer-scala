package utilities

import org.scalatest.funsuite.AnyFunSuite
import utilities.Functions.{injections, partialBijections}

class FunctionsTests extends AnyFunSuite {
  val A = Set(1,2)
  val B = Set(1,2,3)
  val E = Set.empty[Int]
  test("injections") {
    assertResult(6) {injections(A,B).size}
    assertResult(0) {injections(B,A).size}
    assertResult(1) {injections(E,A).size}
    assertResult(0) {injections(A,E).size}
  }

  test("partial bijections") {
    assertResult(13) {partialBijections(A,B).size}
    assertResult(13) {partialBijections(B,A).size}
    assertResult(1) {partialBijections(E,A).size}
    assertResult(1) {partialBijections(A,E).size}
  }
}
