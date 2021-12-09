package modules

import algebras.Sign.{Negative, Positive}
import org.scalatest.funsuite.AnyFunSuite
import tangles.ETangle
import tangles.ETangleType.Straight

class ECTMinusTests extends AnyFunSuite {
  private val straight = new ETangle(Straight, IndexedSeq(Positive, Negative), 0)
  private val ectMinus = new ECTMinus(straight)

  test("left/right algebras") {
    assertResult(IndexedSeq("u0")) {ectMinus.leftAlgebra.ring.varNames}
    assertResult(IndexedSeq("u0")) {ectMinus.rightAlgebra.ring.varNames}
  }

  test("rings") {
    assertResult(IndexedSeq("u0", "u1", "u2")) {ectMinus.ring.varNames}
  }
}