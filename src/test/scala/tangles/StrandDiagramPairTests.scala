package tangles

import algebras.{AMinus, Z2PolynomialRing}
import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite
import tangles.StrandUtils.VariableStrand

class StrandDiagramPairTests extends AnyFunSuite {
  private val R = new Z2PolynomialRing(IndexedSeq("u0"))
  private val u0 = R("u0")
  private val m = new StrandDiagramSpan(R)
  private val mPair = new StrandDiagramPairSpan(R)
  private val x = new StrandDiagram(m, Set(0f -> 0f, 1f -> 1f), Set(VariableStrand(0.5f, 0.5f, Negative, u0)))
  private val y = new StrandDiagram(m, Set(0f -> 1f, 1f -> 0f), Set(VariableStrand(0.5f, 0.5f, Negative, u0)))
  private val e = new StrandDiagram(m, Set(), Set(VariableStrand(0.5f, 0.5f, Negative, u0)))
  private val xe = new StrandDiagramPair(mPair, x, e).toElement
  private val ye = new StrandDiagramPair(mPair, y, e).toElement
  private val ex = new StrandDiagramPair(mPair, e, x).toElement
  private val ey = new StrandDiagramPair(mPair, e, y).toElement

  test("delta_A case 1") {
    assertResult(u0 *: ye) {xe.deltaA}
    assertResult(mPair.zero) {ye.deltaA}
    assertResult(mPair.zero) {ex.deltaA}
    assertResult(mPair.zero) {ey.deltaA}
  }
}
