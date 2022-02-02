package modules

import org.scalatest.funsuite.AnyFunSuite
import algebras.Sign.{Negative, Positive}
import tangles.ETangle
import tangles.ETangleType.{Cap, Cup, Straight}
import modules.TensorProducts.{TypeAATensorProducts, TypeADTensorProducts, TypeDATensorProducts, TypeDDTensorProducts}
import utilities.ModuleRenderer

class ExamplesTests extends AnyFunSuite {
  private val cup = new ETangle(Cup, IndexedSeq(Negative, Positive), 0)
  private val cupDD = CDTD.from(cup)
  private val cupAA = CATA.from(cup)
  private val cupDA = cupDD <*> cupAA

  private val cap = new ETangle(Cap, IndexedSeq(Negative, Positive), 0)
  private val capDD = CDTD.from(cap)
  private val capAA = CATA.from(cap)
  private val capDA = capDD <*> capAA

  private val unknotDA = cupDA <*> capDA

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

  test("new examples") {
    val p = new ETangle(Straight, IndexedSeq(Positive), 0)
    val pAA = CATA.from(p)
    val pDD = CDTD.from(p)
    val pAADDAA = (pAA <*> pDD) <*> pAA
    reflect.io.File("out/pAA.dot").writeAll(ModuleRenderer.render(pAA))
    reflect.io.File("out/pAADDAA.dot").writeAll(ModuleRenderer.render(pAADDAA))

    val pp = new ETangle(Straight, IndexedSeq(Positive, Positive), 0)
    val ppAA = CATA.from(pp)
    reflect.io.File("out/ppAA.dot").writeAll(ModuleRenderer.render(ppAA))
  }
}
