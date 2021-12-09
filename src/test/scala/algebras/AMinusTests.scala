package algebras

import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite

class AMinusTests extends AnyFunSuite {
  private val A = new AMinus(IndexedSeq(Positive))
  private val R = A.ring
  private val u0 = R("u0")
  private val x = A.elt(Map(0f -> 0f, 1f -> 1f))
  private val y = A.elt(Map(0f -> 1f, 1f -> 0f))
  private val a = A.elt(Map(0f -> 0f))
  private val b = A.elt(Map(0f -> 1f))
  private val c = A.elt(Map(1f -> 1f))
  private val d = A.elt(Map(1f -> 0f))

  private val B = new AMinus(IndexedSeq(Negative))
  private val S = B.ring
  private val xB = B.elt(Map(0f -> 0f, 1f -> 1f))
  private val yB = B.elt(Map(0f -> 1f, 1f -> 0f))

  test("differential") {
    assertResult(A.zero) {x.d}
    assertResult(u0 *: x) {y.d}
    assertResult(u0 *: x) {(x + y).d}

    assertResult(A.zero) {a.d}
    assertResult(A.zero) {b.d + c.d + d.d}

    assertResult(B.zero) {xB.d}
    assertResult(B.zero) {yB.d}
  }

  test("multiplication") {
    assertResult(x) {x * x}
    assertResult(y) {x * y}
    assertResult(A.zero) {y * y}

    assertResult(a) {a * a}
    assertResult(c) {c * c}
    assertResult(A.zero) {a * c}
    assertResult(A.zero) {c * a}
    assertResult(u0 *: a) {b * d}
    assertResult(u0 *: c) {d * b}
    assertResult(b) {a * b * c}
    assertResult(d) {c * d * a}

    assertResult(xB) {xB * xB}
    assertResult(B.zero) {yB * yB}
  }
}