package algebra

import algebra.Sign.Positive
import org.scalatest.funsuite.AnyFunSuite

class AMinusTests extends AnyFunSuite {
  private val a = new AMinus(IndexedSeq(Positive))
  private val r = a.ring
  private val u0 = r("u0")
  private val x = a.elt(Set((0, 0), (1, 1)))
  private val y = a.elt(Set((0, 1), (1, 0)))

  test("differential") {
    assertResult(a.zero) {x.d}
    assertResult(u0 *: x) {y.d}
    assertResult(u0 *: x) {(x + y).d}
  }
}