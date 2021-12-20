package utilities

import scalax.collection.io.dot._
import implicits._
import modules.{Module, TypeAA, TypeDA, TypeDD}
import modules.Module.{EdgeLabel, Generator}
import scalax.collection.GraphPredef.Param
import scalax.collection.edge.LkDiEdge
import scalax.collection.Graph

object ModuleRenderer {
  def render[M <: Module[M,L],L](module: Module[M,L], showIdempotents: Boolean = true): String = {
    val graph: Graph[Generator[M,L],LkDiEdge] =
      if (!showIdempotents) {
        def edgeFilter(p: Param[Generator[M,L],LkDiEdge]): Boolean = p match {
          case innerEdge: Graph[Generator[M,L],LkDiEdge]#EdgeT => innerEdge.edge match {
            case LkDiEdge (_, _, EdgeLabel(left, coefficient, right)) =>
              !module.companion.isIdempotentAction(left, coefficient, right)
          }
          case _ => true
        }
        module.graph.filter(edgeFilter)
      } else {
        module.graph
      }

    val root = DotRootGraph (
      directed = true,
      id        = Some("Module"),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "record")))),
      attrList  = List()
    )

    def edgeTransformer(innerEdge: Graph[Generator[M,L],LkDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
      innerEdge.edge match {
        case LkDiEdge(source, target, label) => label match {
          case EdgeLabel(left, _, right) =>
              Some((root,
                DotEdgeStmt(source.toString,
                  target.toString,
                  List(DotAttr("label", label.toString),
                    DotAttr("color", edgeColor(source.value.module, left.factors.length, right.factors.length))))))
        }
      }
    }

    def nodeTransformer(innerNode: Graph[Generator[M,L],LkDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
      Some((root, DotNodeStmt(NodeId(innerNode.value.toString), Seq.empty[DotAttr])))

    graph.toDot(root, edgeTransformer, iNodeTransformer=Some(nodeTransformer))
  }

  def edgeColor(module: Module[_,_], sizeLeft: Int, sizeRight: Int): String = module match {
    case _: TypeAA[_] => Seq("black", "blue", "red", "green", "purple")((sizeLeft + sizeRight) min 4)
    case _: TypeDA[_] => Seq("black", "blue", "red", "green", "purple")(sizeRight min 4)
    case _: TypeDD[_] => "black"
  }
}
