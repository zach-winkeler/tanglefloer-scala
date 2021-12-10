package utilities

import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.io.dot._
import implicits._
import modules.Module
import modules.Module.{EdgeLabel, Generator}
import scalax.collection.edge.LkDiEdge

object ModuleRenderer {
  def render[M <: Module[M]](module: Module[M]): String = {
    val root = DotRootGraph (
      directed = true,
      id        = Some("Module"),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "record")))),
      attrList  = List()
    )
    def edgeTransformer(innerEdge: Graph[Generator[M],LkDiEdge]#EdgeT):
    Option[(DotGraph,DotEdgeStmt)] = innerEdge.edge match {
      case LkDiEdge(source, target, label) => label match {
        case EdgeLabel(left, coefficient, right) =>
          Some((root,
            DotEdgeStmt(source.toString,
              target.toString,
              List(DotAttr("label", label.toString)))))
      }}
    module.graph.toDot(root, edgeTransformer)
  }
}
