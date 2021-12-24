package modules

import algebras.Sign.Positive
import modules.TensorProducts.TypeDDTensorProducts
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.Straight

class TensorProductTests extends AnyFunSuite {
  private val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  private val straightDD = CDTD.from(straight)
  private val straightAA = CATA.from(straight)
  private val straightDA = straightDD <*> straightAA

  test("tensor DD and AA for straight strands") {
    assertResult(12) {straightDA.graph.nodes.size}
    assertResult(31) {straightDA.graph.edges.size}
  }
}
