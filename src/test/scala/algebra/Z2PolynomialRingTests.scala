package algebra

import org.scalatest.funsuite.AnyFunSuite

class Z2PolynomialRingTests extends AnyFunSuite {
  private val r = new Z2PolynomialRing(IndexedSeq("x", "y"))
  private val x = r("x")
  private val y = r("y")
  private val z = new Z2Monomial(r, Map("x" -> 1, "y" -> 1)).toPolynomial

  private val s = new Z2PolynomialRing(IndexedSeq("u", "v", "w"))
  private val u = s("u")
  private val v = s("v")
  private val w = s("w")

  private val f = new Z2PolynomialRingMap(r, s, Map("x" -> "u", "y" -> "v"))
  private val g = new Z2PolynomialRingMap(s, r, Map("u" -> "x", "v" -> "y", "w" -> "y"))

  test("characteristic 2") {
    assertResult(r.zero) {r.one + r.one}
  }

  test("addition") {
    assertResult(y) {x + y + x}
  }

  test("multiplication") {
    assertResult(z) {x * y}
    assertResult(x * x + y * y) {(x + y) * (x + y)}
  }

  test("degree") {
    assertResult(3) {(x * y * x).degree}
  }

  test("application") {
    assertResult(u) {f.apply(x)}
    assertResult(u * v) {f.apply(x * y)}
    assertResult(y * y) {g.apply(v * w)}
  }

  test("toString") {
    assertResult("0") {r.zero.toString}
    assertResult("1") {r.one.toString}
  }
}