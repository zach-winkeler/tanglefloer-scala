package modules

import modules.Module.{EdgeLabel, Generator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

object Reduction {
  implicit class TypeDAReduction[L](base: TypeDA[L]) {
    def reduce(): Unit = {
      var eOpt = getReducibleEdge
      while (eOpt.nonEmpty) {
        reduceEdge(eOpt.get)
        eOpt = getReducibleEdge
      }
    }

    def reduceEdge(e: LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]): Unit = {
      val LkDiEdge(_x, _y, EdgeLabel(left, c, right)) = e
      val x = _x.value
      val y = _y.value
      val wys = y.incoming
      val xzs = x.outgoing

      base.graph = base.graph - x - y

      for (LkDiEdge(_w, _, EdgeLabel(wyLeft, wyC, wyRight)) <- wys if (_w.value != x) && (_w.value != y);
           LkDiEdge(_, _z, EdgeLabel(xzLeft, xzC, xzRight)) <- xzs if (_z.value != x) && (_z.value != y)) {
        val w = _w.value
        val z = _z.value
        base.addStructureMap(((w :<*> wyRight).forceGen :<*> xzRight).forceGen,
          (wyC * xzC) *: ((wyLeft.factors(0) * left.factors(0) * xzLeft.factors(0).toElement) <*>: z.toElement))
      }
    }

    def getReducibleEdge: Option[LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]] =
      base.graph.edges.view.map(_.edge).find(isReducibleEdge)

    def isReducibleEdge(e: LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]): Boolean =
      e match {case LkDiEdge(_, _, EdgeLabel(left, c, right)) =>
        left.factors(0).isIdempotent && (c == c.ring.one) && right.factors.isEmpty &&
          (e.from.outgoing.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]]] &
            e.to.incoming.asInstanceOf[Set[LkDiEdge[Graph[Generator[TypeDA[L],L],LkDiEdge]#NodeT]]]).size == 1}
  }
}
