package modules

import algebras.{TensorAlgebra, Z2PolynomialRing}
import modules.Module.{EdgeLabel, Generator, NodeLabel, TensorElement, TensorGenerator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

abstract class Reducible[M <: Module[M, K], K](base: M) {
  def reduce(): Unit = {
    var eOpt = getReducibleEdge
    while (eOpt.nonEmpty) {
      print(s"Reducing... Nodes: ${base.graph.nodes.size}, Edges: ${base.graph.edges.size}\n")
      reduceEdge(base, eOpt.get)
      eOpt = getReducibleEdge
    }
  }

  def getReducibleEdge: Option[LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]] =
    base.graph.edges.view.map(_.edge).find(isReducibleEdge)

  def isReducibleEdge(e: LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]): Boolean

  def reduceEdge(m: M, e: LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]): Unit = {
    val LkDiEdge(_x, _y, _) = e
    val x = m.gen(_x.value)
    val y = m.gen(_y.value)
    val wys = y.incoming
    val xzs = x.outgoing

    base.graph = base.graph - x.label - y.label

    for (LkDiEdge(_w, _, EdgeLabel(wyLeft, wyC, wyRight)) <- wys if (_w.value != x.label) && (_w.value != y.label);
         LkDiEdge(_, _z, EdgeLabel(xzLeft, xzC, xzRight)) <- xzs if (_z.value != x.label) && (_z.value != y.label)) {
      val w = m.gen(_w.value)
      val z = m.gen(_z.value)
      (base.addStructureMap _).tupled(reducedStructureMap(w, wyLeft, wyC, wyRight, z, xzLeft, xzC, xzRight))
    }
  }

  def reducedStructureMap(w: Generator[M,K],
                          wyLeft: TensorAlgebra.Generator,
                          wyC: Z2PolynomialRing.Element,
                          wyRight: TensorAlgebra.Generator,
                          z: Generator[M, K],
                          xzLeft: TensorAlgebra.Generator,
                          xzC: Z2PolynomialRing.Element,
                          xzRight: TensorAlgebra.Generator): (TensorGenerator[M, K], TensorElement[M, K])
}

object Reduction {
  implicit class TypeDAReduction[K](base: TypeDA[K]) extends Reducible[TypeDA[K], K](base) {
    def isReducibleEdge(e: LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]): Boolean =
      e match { case LkDiEdge(_, _, EdgeLabel(left, c, right)) =>
        left.factors(0).isIdempotent && (c == c.ring.one) && right.factors.isEmpty &&
          (e.from.outgoing.asInstanceOf[Set[LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]]] &
            e.to.incoming.asInstanceOf[Set[LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]]]).size == 1 }

    def reducedStructureMap(w: Generator[TypeDA[K],K],
                            wyLeft: TensorAlgebra.Generator,
                            wyC: Z2PolynomialRing.Element,
                            wyRight: TensorAlgebra.Generator,
                            z: Generator[TypeDA[K], K],
                            xzLeft: TensorAlgebra.Generator,
                            xzC: Z2PolynomialRing.Element,
                            xzRight: TensorAlgebra.Generator):
    (TensorGenerator[TypeDA[K], K], TensorElement[TypeDA[K], K]) =
      (((w :<*> wyRight).forceGen :<*> xzRight).forceGen,
        (wyC * xzC) *: ((wyLeft.factors(0) * xzLeft.factors(0)) <*>: z.toElement))
  }

  implicit class TypeAAReduction[K](base: TypeAA[K]) extends Reducible[TypeAA[K], K](base){
    def isReducibleEdge(e: LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]): Boolean =
      e match { case LkDiEdge(_, _, EdgeLabel(left, c, right)) =>
        left.factors.isEmpty && (c == c.ring.one) && right.factors.isEmpty &&
          (e.from.outgoing.asInstanceOf[Set[LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]]] &
            e.to.incoming.asInstanceOf[Set[LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]]]).size == 1 }

    def reducedStructureMap(w: Generator[TypeAA[K],K],
                            wyLeft: TensorAlgebra.Generator,
                            wyC: Z2PolynomialRing.Element,
                            wyRight: TensorAlgebra.Generator,
                            z: Generator[TypeAA[K], K],
                            xzLeft: TensorAlgebra.Generator,
                            xzC: Z2PolynomialRing.Element,
                            xzRight: TensorAlgebra.Generator):
    (TensorGenerator[TypeAA[K], K], TensorElement[TypeAA[K], K]) =
      (((xzLeft <*>: (wyLeft <*>: (w :<*> wyRight).forceGen).forceGen).forceGen :<*> xzRight).forceGen,
        (wyC * xzC) *: z.toElement)
  }

  implicit class TypeDDReduction[K](base: TypeDD[K]) extends Reducible[TypeDD[K], K](base) {
    def isReducibleEdge(e: LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]): Boolean =
      e match { case LkDiEdge(_, _, EdgeLabel(left, c, right)) =>
        left.factors(0).isIdempotent && (c == c.ring.one) && right.factors(0).isIdempotent &&
          (e.from.outgoing.asInstanceOf[Set[LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]]] &
            e.to.incoming.asInstanceOf[Set[LkDiEdge[Graph[NodeLabel[K],LkDiEdge]#NodeT]]]).size == 1 }

    def reducedStructureMap(w: Generator[TypeDD[K],K],
                            wyLeft: TensorAlgebra.Generator,
                            wyC: Z2PolynomialRing.Element,
                            wyRight: TensorAlgebra.Generator,
                            z: Generator[TypeDD[K], K],
                            xzLeft: TensorAlgebra.Generator,
                            xzC: Z2PolynomialRing.Element,
                            xzRight: TensorAlgebra.Generator):
    (TensorGenerator[TypeDD[K], K], TensorElement[TypeDD[K], K]) =
      (w, (wyC * xzC) *:
        (wyLeft.factors(0) * xzLeft.factors(0) <*>: z.toElement :<*> xzRight.factors(0) * wyRight.factors(0)))
  }
}
