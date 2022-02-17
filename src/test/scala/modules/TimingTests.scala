package modules

import algebras.Sign.{Negative, Positive}
import modules.Reduction.{TypeAAReduction, TypeDAReduction, TypeDDReduction}
import modules.TensorProducts.TypeDDTensorProducts
import org.scalatest.Outcome
import org.scalatest.funsuite.AnyFunSuite
import tangles.{ETangle, Strand}
import tangles.ETangleType.{Straight, Cup}

class TimingTests extends AnyFunSuite {
  private var t: ETangle = null
  private var tDD: TypeDD[Set[Strand]] = null
  private var tAA: TypeAA[Set[Strand]] = null

  override protected def withFixture(test: NoArgTest): Outcome = {
    t = new ETangle(Straight, IndexedSeq(Positive, Negative), 0)
    tDD = CDTD.from(t)
    tAA = CATA.from(t)
    test()
  }

  test("don't reduce DD or AA before tensoring") {
    val tDA = tDD <*> tAA
    tDA.reduce()
  }

  test("do reduce DD or AA before tensoring") {
    tDD.reduce()
    tAA.reduce()
    val tDA = tDD <*> tAA
    tDA.reduce()
  }
}
