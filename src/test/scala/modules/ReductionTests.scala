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

  test("cup DA reduction") {
    val e = cupDA.getReducibleEdge.get
    print(e)
    cupDA.reduceEdge(e)
    reflect.io.File("out/cupDAReducedOnce.dot").writeAll(ModuleRenderer.render(cupDA, true))
    reflect.io.File("out/cupDAReduced.dot").writeAll(ModuleRenderer.render(cupDA, true))
  }

  test("unknot DA reduction") {
    unknotDA.reduce()
    reflect.io.File("out/unknotDAReduced.dot").writeAll(ModuleRenderer.render(unknotDA, true))
  }
}
