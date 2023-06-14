package modules

import algebras.Sign.{Negative, Positive}
import modules.Module.EdgeLabel
import modules.Reduction.TypeAAReduction
import modules.TensorProducts.{TypeAATensorProducts, TypeADTensorProducts}
import org.scalatest.funsuite.AnyFunSuite
import scalax.collection.edge.LkDiEdge
import tangles.ETangle
import tangles.ETangleType.Straight
import utilities.ModuleRenderer

class EmptyBoxesTests extends AnyFunSuite {

  test("reduce empty boxes") {
    val t = new ETangle(Straight, IndexedSeq(Negative), 0)
    val tDD = CDTD.from(t)
    val tAA = CATA.from(t)
    val tAADDAA = tAA <*> tDD <*> tAA

    reflect.io.File("out/emptyBoxes0.dot").writeAll(ModuleRenderer.render(tAADDAA, onlyDifferentials=true))
    tAADDAA.graph.edges.view.map(_.edge).find(e =>
      e match {
        case LkDiEdge(s, t, EdgeLabel(left, c, right)) =>
          s.toString().equals("(((0 -> 0),(1 -> 0)),(1 -> 1))") &&
            t.toString().equals("(((0 -> 1),(0 -> 0)),(1 -> 1))") &&
            left.factors.isEmpty &&
            c.equals(c.ring.one) &&
            right.factors.isEmpty
      }).foreach(e => {print(e+"\n"); tAADDAA.reduceEdge(tAADDAA, e)})
    tAADDAA.graph.edges.view.map(_.edge).find(e =>
      e match {
        case LkDiEdge(s, t, EdgeLabel(left, c, right)) =>
          s.toString().equals("(((0 -> 0),(1 -> 0)),(1 -> 0))") &&
            t.toString().equals("(((0 -> 1),(0 -> 0)),(1 -> 0))") &&
            left.factors.isEmpty &&
            c.equals(c.ring.one) &&
            right.factors.isEmpty
      }).foreach(e => {print(e+"\n"); tAADDAA.reduceEdge(tAADDAA, e)})
    tAADDAA.graph.edges.view.map(_.edge).find(e =>
      e match {
        case LkDiEdge(s, t, EdgeLabel(left, c, right)) =>
          s.toString().equals("(((1 -> 1),(0 -> 1)),(0 -> 0))") &&
            t.toString().equals("(((1 -> 1),(0 -> 0)),(1 -> 0))") &&
            left.factors.isEmpty &&
            c.equals(c.ring.one) &&
            right.factors.isEmpty
      }).foreach(e => {print(e+"\n"); tAADDAA.reduceEdge(tAADDAA, e)})
    tAADDAA.graph.edges.view.map(_.edge).find(e =>
      e match {
        case LkDiEdge(s, t, EdgeLabel(left, c, right)) =>
          s.toString().equals("(((0 -> 1),(0 -> 1)),(0 -> 0))") &&
            t.toString().equals("(((0 -> 1),(0 -> 0)),(1 -> 0))") &&
            left.factors.isEmpty &&
            c.equals(c.ring.one) &&
            right.factors.isEmpty
      }).foreach(e => {print(e+"\n"); tAADDAA.reduceEdge(tAADDAA, e)})
    reflect.io.File("out/emptyBoxes1.dot").writeAll(ModuleRenderer.render(tAADDAA, onlyDifferentials=true))
  }
}
