package modules

import algebras.{TensorAlgebra, Z2PolynomialRing}
import modules.Module.{EdgeLabel, Generator, TensorElement, TensorGenerator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

abstract class Reducible[M <: Module[M, L], L](base: M) {
  def reduce(): Unit = {
    var eOpt = getReducibleEdge
    while (eOpt.nonEmpty) {
      reduceEdge(eOpt.get)
      eOpt = getReducibleEdge
    }
  }

  def getReducibleEdge: Option[LkDiEdge[Graph[Generator[M,L],LkDiEdge]#NodeT]] =
    base.graph.edges.view.map(_.edge).find(isReducibleEdge)

  def isReducibleEdge(e: LkDiEdge[Graph[Generator[M,L],LkDiEdge]#NodeT]): Boolean

  def reduceEdge(e: LkDiEdge[Graph[Generator[M,L],LkDiEdge]#NodeT]): Unit = {
    val LkDiEdge(_x, _y, _) = e
    val x = _x.value
    val y = _y.value
    val wys = y.incoming
    val xzs = x.outgoing

    base.graph = base.graph - x - y

    for (LkDiEdge(_w, _, EdgeLabel(wyLeft, wyC, wyRight)) <- wys if (_w.value != x) && (_w.value != y);
         LkDiEdge(_, _z, EdgeLabel(xzLeft, xzC, xzRight)) <- xzs if (_z.value != x) && (_z.value != y)) {
      val w = _w.value
      val z = _z.value
      (base.addStructureMap _).tupled(reducedStructureMap(w, wyLeft, wyC, wyRight, z, xzLeft, xzC, xzRight))
    }
  }

  def reducedStructureMap(w: Generator[M,L],
                          wyLeft: TensorAlgebra.Generator,
                          wyC: Z2PolynomialRing.Element,
                          wyRight: TensorAlgebra.Generator,
                          z: Generator[M, L],
                          xzLeft: TensorAlgebra.Generator,
                          xzC: Z2PolynomialRing.Element,
                          xzRight: TensorAlgebra.Generator): (TensorGenerator[M, L], TensorElement[M, L])
}

object Reduction {
  implicit class TypeDAReduction[L](base: TypeDA[L]) extends Reducible[TypeDA[L], L](base) {
    def isReducibleEdge(e: LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]): Boolean =
      e match { case LkDiEdge(_, _, EdgeLabel(left, c, right)) =>
        left.factors(0).isIdempotent && (c == c.ring.one) && right.factors.isEmpty &&
          (e.from.outgoing.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]]] &
            e.to.incoming.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]]]).size == 1 }

    def reducedStructureMap(w: Generator[TypeDA[L],L],
                            wyLeft: TensorAlgebra.Generator,
                            wyC: Z2PolynomialRing.Element,
                            wyRight: TensorAlgebra.Generator,
                            z: Generator[TypeDA[L], L],
                            xzLeft: TensorAlgebra.Generator,
                            xzC: Z2PolynomialRing.Element,
                            xzRight: TensorAlgebra.Generator):
    (TensorGenerator[TypeDA[L], L], TensorElement[TypeDA[L], L]) =
      (((w :<*> wyRight).forceGen :<*> xzRight).forceGen,
        (wyC * xzC) *: ((wyLeft.factors(0) * xzLeft.factors(0)) <*>: z.toElement))
  }

  implicit class TypeAAReduction[L](base: TypeAA[L]) extends Reducible[TypeAA[L], L](base){
    def isReducibleEdge(e: LkDiEdge[Graph[Generator[TypeAA[L],L],LkDiEdge]#NodeT]): Boolean =
      e match { case LkDiEdge(_, _, EdgeLabel(left, c, right)) =>
        left.factors.isEmpty && (c == c.ring.one) && right.factors.isEmpty &&
          (e.from.outgoing.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeAA[L],L],LkDiEdge]#NodeT]]] &
            e.to.incoming.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeAA[L],L],LkDiEdge]#NodeT]]]).size == 1 }

    def reducedStructureMap(w: Generator[TypeAA[L],L],
                            wyLeft: TensorAlgebra.Generator,
                            wyC: Z2PolynomialRing.Element,
                            wyRight: TensorAlgebra.Generator,
                            z: Generator[TypeAA[L], L],
                            xzLeft: TensorAlgebra.Generator,
                            xzC: Z2PolynomialRing.Element,
                            xzRight: TensorAlgebra.Generator):
    (TensorGenerator[TypeAA[L], L], TensorElement[TypeAA[L], L]) =
      (((xzLeft <*>: (wyLeft <*>: (w :<*> wyRight).forceGen).forceGen).forceGen :<*> xzRight).forceGen,
        (wyC * xzC) *: z.toElement)
  }

  implicit class TypeDDReduction[L](base: TypeDD[L]) extends Reducible[TypeDD[L], L](base) {
    def isReducibleEdge(e: LkDiEdge[Graph[Generator[TypeDD[L],L],LkDiEdge]#NodeT]): Boolean =
      e match { case LkDiEdge(_, _, EdgeLabel(left, c, right)) =>
        left.factors(0).isIdempotent && (c == c.ring.one) && right.factors(0).isIdempotent &&
          (e.from.outgoing.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeDD[L],L],LkDiEdge]#NodeT]]] &
            e.to.incoming.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeDD[L],L],LkDiEdge]#NodeT]]]).size == 1 }

    def reducedStructureMap(w: Generator[TypeDD[L],L],
                            wyLeft: TensorAlgebra.Generator,
                            wyC: Z2PolynomialRing.Element,
                            wyRight: TensorAlgebra.Generator,
                            z: Generator[TypeDD[L], L],
                            xzLeft: TensorAlgebra.Generator,
                            xzC: Z2PolynomialRing.Element,
                            xzRight: TensorAlgebra.Generator):
    (TensorGenerator[TypeDD[L], L], TensorElement[TypeDD[L], L]) =
      (w, (wyC * xzC) *:
        (wyLeft.factors(0) * xzLeft.factors(0) <*>: z.toElement :<*> xzRight.factors(0) * wyRight.factors(0)))
  }
}
