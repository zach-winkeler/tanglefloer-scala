package modules

import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.{Cap, Cup, Straight}
import utilities.ModuleRenderer

class CDTDTests extends AnyFunSuite {
  val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  val straightDD = CDTD.from(straight)

  val straightN = new ETangle(Straight, IndexedSeq(Negative), 0)
  val straightNDD = CDTD.from(straightN)

  val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  val cupDD = CDTD.from(cup)

  val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  val capDD = CDTD.from(cap)

  test("straight type DD") {
    assertResult(7) {straightDD.graph.nodes.size}
    reflect.io.File("out/straightDD.dot").writeAll(ModuleRenderer.render(straightDD, true))
  }

  test("straight negative type DD") {
    assertResult(7) {straightNDD.graph.nodes.size}
    reflect.io.File("out/straightNDD.dot").writeAll(ModuleRenderer.render(straightNDD, false))
  }

  test("cup type DD") {
    assertResult(3) {cupDD.graph.nodes.size}
    reflect.io.File("out/cupDD.dot").writeAll(ModuleRenderer.render(cupDD, false))
  }

  test("cap type DD") {
    assertResult(13) {capDD.graph.nodes.size}
    reflect.io.File("out/capDD.dot").writeAll(ModuleRenderer.render(capDD, false))
  }
}
