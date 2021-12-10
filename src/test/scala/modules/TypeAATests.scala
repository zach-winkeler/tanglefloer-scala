package modules

import algebras.AMinus
import algebras.Sign.Positive
import modules.TypeAA.AMinusExtensions
import org.scalatest.funsuite.AnyFunSuite
import utilities.ModuleRenderer

class TypeAATests extends AnyFunSuite {
  private val A = new AMinus(IndexedSeq(Positive))
  private val typeAA = A.asTypeAA

  test("type AA") {
    assertResult(7) {typeAA.graph.nodes.size}
    println(ModuleRenderer.render(typeAA))
  }
}
