package modules

import algebras.Sign.{Negative, Positive}
import modules.TensorProducts.TypeDDTensorProducts
import modules.TensorProducts.TypeDATensorProducts
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.{Straight, Cup, Cap}
import utilities.ModuleRenderer

class TensorProductTests extends AnyFunSuite {
  val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  val straightDD = CDTD.from(straight)
  val straightAA = CATA.from(straight)
  val straightDA = straightDD <*> straightAA

  val straightN = new ETangle(Straight, IndexedSeq(Negative), 0)
  val straightNDD = CDTD.from(straightN)
  val straightNAA = CATA.from(straightN)
  val straightNDA = straightNDD <*> straightNAA

  val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  val cupDD = CDTD.from(cup)
  val cupAA = CATA.from(cup)
  val cupDA = cupDD <*> cupAA

  val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  val capDD = CDTD.from(cap)
  val capAA = CATA.from(cap)
  val capDA = capDD <*> capAA

  val unknotDA = cupDA <*> capDA

  test("tensor DD and AA for straight strands") {
    assertResult(12) {straightDA.graph.nodes.size}
//    assertResult(26) {straightDA.graph.edges.size}
    reflect.io.File("out/straightDA.dot").writeAll(ModuleRenderer.render(straightDA, true))
    reflect.io.File("out/straightNDA.dot").writeAll(ModuleRenderer.render(straightNDA, false))
  }

  test("tensor DD and AA for cup/cap") {
    reflect.io.File("out/cupDA.dot").writeAll(ModuleRenderer.render(cupDA, false))
    reflect.io.File("out/capDA.dot").writeAll(ModuleRenderer.render(capDA, false))
    reflect.io.File("out/unknotDA.dot").writeAll(ModuleRenderer.render(unknotDA, false))
  }
}
