package modules

import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.{Straight, Cup, Cap}
import utilities.ModuleRenderer

class CATATests extends AnyFunSuite {
  val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  val straightAA = CATA.from(straight)

  val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  val cupAA = CATA.from(cup)

  val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  val capAA = CATA.from(cap)

  test("straight type AA") {
    assertResult(7) {straightAA.graph.nodes.size}
    reflect.io.File("out/straightAA.dot").writeAll(ModuleRenderer.render(straightAA, true))
  }

  test("cup type AA") {
    assertResult(13) {cupAA.graph.nodes.size}
    reflect.io.File("out/cupAA.dot").writeAll(ModuleRenderer.render(cupAA, false))
  }

  test("cap type AA") {
    assertResult(3) {capAA.graph.nodes.size}
    reflect.io.File("out/capAA.dot").writeAll(ModuleRenderer.render(capAA, false))
  }
}
