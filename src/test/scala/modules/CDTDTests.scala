package modules

import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.{Cap, Cup, Straight}

class CDTDTests extends AnyFunSuite {
  private val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  private val straightDD = CDTD.from(straight)

  private val straightN = new ETangle(Straight, IndexedSeq(Negative), 0)
  private val straightNDD = CDTD.from(straightN)

  private val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  private val cupDD = CDTD.from(cup)

  private val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  private val capDD = CDTD.from(cap)

  test("straight type DD") {
    assertResult(7) {straightDD.graph.nodes.size}
  }

  test("straight negative type DD") {
    assertResult(7) {straightNDD.graph.nodes.size}
  }

  test("cup type DD") {
    assertResult(3) {cupDD.graph.nodes.size}
  }

  test("cap type DD") {
    assertResult(13) {capDD.graph.nodes.size}
  }
}
