package modules

import org.scalatest.funsuite.AnyFunSuite
import algebras.Sign.{Negative, Positive}
import tangles.ETangle
import tangles.ETangleType.{Cap, Cup}
import modules.TensorProducts.TypeDDTensorProducts
import modules.TensorProducts.TypeDATensorProducts
import modules.Reduction.TypeDAReduction

class ReductionTests extends AnyFunSuite {
  private val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  private val cupDD = CDTD.from(cup)
  private val cupAA = CATA.from(cup)
  private val cupDA = cupDD <*> cupAA

  private val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  private val capDD = CDTD.from(cap)
  private val capAA = CATA.from(cap)
  private val capDA = capDD <*> capAA

  private val unknotDA = cupDA <*> capDA

  test("unknot DA reduction") {
    assertResult(24) {unknotDA.graph.nodes.size}
    unknotDA.reduce()
    assertResult(8) {unknotDA.graph.nodes.size}
  }
}
