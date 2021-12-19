package modules

import algebras.Sign.{Negative, Positive}
import modules.TensorProducts.TypeDDTensorProducts
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.{Straight, Cup, Cap}
import utilities.ModuleRenderer

class TensorProductTests extends AnyFunSuite {
  val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  val straightDD = CDTD.from(straight)
  val straightAA = CATA.from(straight)
  val straightDA = straightDD <*> straightAA

  val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  val cupDD = CDTD.from(cup)
  val cupAA = CATA.from(cup)
  val cupDA = cupDD <*> cupAA
  val capDD = CDTD.from(cap)
  val capAA = CATA.from(cap)
  val capDA = capDD <*> capAA

  test("tensor DD and AA for straight strands") {
    assertResult(12) {straightDA.graph.nodes.size}
    reflect.io.File("out/straightDA.dot").writeAll(ModuleRenderer.render(straightDA, false))
  }

  test("tensor DD and AA for cup/cap") {
    reflect.io.File("out/cupDA.dot").writeAll(ModuleRenderer.render(cupDA, false))
    reflect.io.File("out/capDA.dot").writeAll(ModuleRenderer.render(capDA, false))
  }
}
