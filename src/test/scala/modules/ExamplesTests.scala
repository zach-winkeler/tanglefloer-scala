package modules

import org.scalatest.funsuite.AnyFunSuite
import algebras.Sign.{Negative, Positive}
import tangles.ETangle
import tangles.ETangleType.{Cap, Cup, Over, Straight, Under}
import modules.TensorProducts.{TypeAATensorProducts, TypeADTensorProducts, TypeDATensorProducts, TypeDDTensorProducts}
import utilities.ModuleRenderer
import modules.Reduction.{TypeAAReduction, TypeDAReduction, TypeDDReduction}

class ExamplesTests extends AnyFunSuite {
  private val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  private val cupDD = CDTD.from(cup)
  cupDD.reduce()
  private val cupAA = CATA.from(cup)
  cupAA.reduce()
  private val cupDA = cupDD <*> cupAA
  cupDA.reduce()

  private val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  private val capDD = CDTD.from(cap)
  capDD.reduce()
  private val capAA = CATA.from(cap)
  capAA.reduce()
  private val capDA = capDD <*> capAA
  capDA.reduce()

  private val unknotDA = cupDA <*> capDA
  unknotDA.reduce()

  private val t = new ETangle(Straight, IndexedSeq(Positive, Positive), 0)
  private val tAA = CATA.from(t)

  private val straight = new ETangle(Straight, IndexedSeq(Positive), 0)
  private val straightDD = CDTD.from(straight)
  private val straightAA = CATA.from(straight)
  private val straightDA = straightDD <*> straightAA

  test("render modules to dot") {
    reflect.io.File("out/cupDD.dot").writeAll(ModuleRenderer.render(cupDD))
    reflect.io.File("out/cupAA.dot").writeAll(ModuleRenderer.render(cupAA))
    reflect.io.File("out/cupDA.dot").writeAll(ModuleRenderer.render(cupDA))
    reflect.io.File("out/capDD.dot").writeAll(ModuleRenderer.render(capDD))
    reflect.io.File("out/capAA.dot").writeAll(ModuleRenderer.render(capAA))
    reflect.io.File("out/capDA.dot").writeAll(ModuleRenderer.render(capDA))
    reflect.io.File("out/unknotDA.dot").writeAll(ModuleRenderer.render(unknotDA))
    reflect.io.File("out/straightDD.dot").writeAll(ModuleRenderer.render(straightDD))
    reflect.io.File("out/straightAA.dot").writeAll(ModuleRenderer.render(straightAA))
    reflect.io.File("out/straightDA.dot").writeAll(ModuleRenderer.render(straightDA))
  }

  test("test") {
    reflect.io.File("out/tAA.dot").writeAll(ModuleRenderer.render(tAA, onlyDifferentials=true))
  }
}
