package algebra

import org.scalatest.funsuite.AnyFunSuite

class AMinusTests extends AnyFunSuite {
  private val a = new AMinus(Vector(Sign.Positive))
  private val r = a.ring
  private val u0 = r("u0")
  private val x = a.elt(Map(0 -> 0, 1 -> 1))
  private val y = a.elt(Map(0 -> 1, 1 -> 0))

  test("multiplication") {
    assertResult(u0 *: x) {y * y}
  }
}