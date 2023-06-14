package modules

import algebras.Sign.{Negative, Positive}
import modules.Module.{EdgeLabel, NodeLabel}
import modules.Reduction.TypeAAReduction
import modules.TensorProducts.{TypeAATensorProducts, TypeADTensorProducts}
import org.scalatest.funsuite.AnyFunSuite
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph
import tangles.ETangle
import tangles.ETangleType.Straight
import utilities.ModuleRenderer

class ExamplesTests extends AnyFunSuite {

  private val p = new ETangle(Straight, IndexedSeq(Positive), 0)
  private val pDD = CDTD.from(p)
  private val pAA = CATA.from(p)
  private val pAADDAA = pAA <*> pDD <*> pAA

  private val n = new ETangle(Straight, IndexedSeq(Negative), 0)
  private val nDD = CDTD.from(n)
  private val nAA = CATA.from(n)
  private val nAADDAA = nAA <*> nDD <*> nAA

  private val nn = new ETangle(Straight, IndexedSeq(Negative, Negative), 0)
  private val nnDD = CDTD.from(nn)
  private val nnAA = CATA.from(nn)
  private val nnAADDAA = nnAA <*> nnDD <*> nnAA

  test("render modules to dot") {
    reflect.io.File("out/pAA.dot").writeAll(ModuleRenderer.render(pAA))
    pAA.reduce()
    reflect.io.File("out/pAAred.dot").writeAll(ModuleRenderer.render(pAA))

    reflect.io.File("out/pAADDAA.dot").writeAll(ModuleRenderer.render(pAADDAA))
    pAADDAA.reduce()
    reflect.io.File("out/pAADDAAred.dot").writeAll(ModuleRenderer.render(pAADDAA))

    reflect.io.File("out/nAA.dot").writeAll(ModuleRenderer.render(nAA))
    nAA.reduce()
    reflect.io.File("out/nAAred.dot").writeAll(ModuleRenderer.render(nAA))

    reflect.io.File("out/nAADDAA.dot").writeAll(ModuleRenderer.render(nAADDAA))
    nAADDAA.reduce()
    reflect.io.File("out/nAADDAAred.dot").writeAll(ModuleRenderer.render(nAADDAA))

    var i = 0
    nnAADDAA.summands.foreach((summand) => {
      reflect.io.File(s"out/nnAADDAA${i}.dot").writeAll(ModuleRenderer.render(summand, onlyDifferentials = true))
      summand.reduce()
      reflect.io.File(s"out/nnAADDAA${i}r.dot").writeAll(ModuleRenderer.render(summand, onlyDifferentials = true))
      i += 1
    })
  }
}
