package modules

import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.{Straight, Cup, Cap}
import utilities.ModuleRenderer

class CATATests extends AnyFunSuite {
  private val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  private val straightAA = CATA.from(straight)

  private val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  private val cupAA = CATA.from(cup)

  private val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  private val capAA = CATA.from(cap)

  test("straight type AA") {
    assertResult(7) {straightAA.graph.nodes.size}
  }

  test("cup type AA") {
    assertResult(13) {cupAA.graph.nodes.size}
  }

  test("cap type AA") {
    assertResult(3) {capAA.graph.nodes.size}
  }
}
