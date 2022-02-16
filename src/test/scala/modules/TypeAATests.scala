package modules

import algebras.AMinus
import algebras.Sign.Positive
import modules.TypeAA.AMinusExtensions
import org.scalatest.funsuite.AnyFunSuite

class TypeAATests extends AnyFunSuite {
  private val A = new AMinus(IndexedSeq(Set(Positive)))
  private val typeAA = A.asTypeAA

  test("type AA") {
    assertResult(7) {typeAA.graph.nodes.size}
  }
}
