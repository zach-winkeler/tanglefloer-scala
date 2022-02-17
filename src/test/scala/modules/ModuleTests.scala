package modules

import algebras.Sign.Positive
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.Straight

class ModuleTests extends AnyFunSuite {
  private val p = new ETangle(Straight, IndexedSeq(Positive), 0)
  private val pAA = CATA.from(p)

  test("finding direct summands") {
    assertResult(3) {pAA.summands.length}
  }

  test("taking direct sums") {
    assertResult(pAA) {Module.directSum(pAA.summands)}
  }
}
