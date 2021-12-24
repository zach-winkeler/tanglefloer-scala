package modules

import org.scalatest.funsuite.AnyFunSuite
import algebras.Sign.{Negative, Positive}
import tangles.ETangle
import tangles.ETangleType.{Cap, Cup}
import modules.TensorProducts.TypeDDTensorProducts
import modules.TensorProducts.TypeDATensorProducts
import modules.Reduction.TypeDAReduction
import utilities.ModuleRenderer

class ReductionTests extends AnyFunSuite {
  val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  val cupDD = CDTD.from(cup)
  val cupAA = CATA.from(cup)
  val cupDA = cupDD <*> cupAA

  val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  val capDD = CDTD.from(cap)
  val capAA = CATA.from(cap)
  val capDA = capDD <*> capAA

  val unknotDA = cupDA <*> capDA

  test("unknot DA reduction") {
    assertResult(24) {unknotDA.graph.nodes.size}
    unknotDA.reduce()
    assertResult(8) {unknotDA.graph.nodes.size}
  }
}
