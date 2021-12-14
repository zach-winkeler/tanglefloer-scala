package tangles

import algebras.{AMinus, Z2PolynomialRing}
import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite
import tangles.StrandUtils.VariableStrand

class StrandDiagramTests extends AnyFunSuite {
  private val R = new Z2PolynomialRing(IndexedSeq("u0"))
  private val u0 = R("u0")
  private val m = new StrandDiagramSpan(R)
  private val x = new StrandDiagram(m, Set(0f -> 0f, 1f -> 1f), Set(VariableStrand(0.5f, 0.5f, Negative, u0))).toElement
  private val y = new StrandDiagram(m, Set(0f -> 1f, 1f -> 0f), Set(VariableStrand(0.5f, 0.5f, Negative, u0))).toElement

  test("d_plus") {
    assertResult(m.zero) {x.dPlus}
    assertResult(m.zero) {y.dPlus}
  }

  test("d_minus") {
    assertResult(u0 *: y) {x.dMinus}
    assertResult(m.zero) {y.dMinus}
  }
}
