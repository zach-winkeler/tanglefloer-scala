package utilities

import scalax.collection.io.dot._
import implicits._
import modules.{Module, TypeAA, TypeDA, TypeDD}
import modules.Module.{EdgeLabel, Generator, NodeLabel}
import scalax.collection.GraphPredef.Param
import scalax.collection.edge.LkDiEdge
import scalax.collection.Graph

object ModuleRenderer {
  def render[M <: Module[M,K],K](module: Module[M,K],
                                 showIdempotents: Boolean = false,
                                 onlyDifferentials: Boolean = false): String = {
    val graph: Graph[NodeLabel[K],LkDiEdge] = {
      if (onlyDifferentials) {
        def edgeFilter(p: Param[NodeLabel[K],LkDiEdge]): Boolean = p match {
          case innerEdge: Graph[NodeLabel[K],LkDiEdge]#EdgeT => innerEdge.edge match {
            case LkDiEdge (_, _, EdgeLabel(left, _, right)) =>
              edgeColor(module, left.factors.length, right.factors.length) == "black"
          }
          case _ => true
        }
        module.graph.filter(edgeFilter)
      } else if (!showIdempotents) {
        def edgeFilter(p: Param[NodeLabel[K],LkDiEdge]): Boolean = p match {
          case innerEdge: Graph[NodeLabel[K],LkDiEdge]#EdgeT => innerEdge.edge match {
            case LkDiEdge (_, _, EdgeLabel(left, coefficient, right)) =>
              !module.companion.isIdempotentAction(left, coefficient, right)
          }
          case _ => true
        }
        module.graph.filter(edgeFilter)
      } else {
        module.graph
      }
    }

    val root = DotRootGraph (
      directed = true,
      id        = Some("Module"),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "record")))),
      attrList  = List()
    )

    def edgeTransformer(innerEdge: Graph[NodeLabel[K],LkDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
      innerEdge.edge match {
        case LkDiEdge(source, target, label) => label match {
          case EdgeLabel(left, _, right) =>
            if (edgeColor(module, left.factors.length, right.factors.length) == "black") {
              Some((root,
                DotEdgeStmt(source.toString,
                  target.toString,
                  List(DotAttr("label", label.toString),
                    DotAttr("color", edgeColor(module, left.factors.length, right.factors.length)),
                    DotAttr("dir", "forward")))))
            } else {
              Some((root,
                DotEdgeStmt(target.toString,
                  source.toString,
                  List(DotAttr("label", label.toString),
                    DotAttr("color", edgeColor(module, left.factors.length, right.factors.length)),
                    DotAttr("dir", "back")))))
            }
          case _ => throw new RuntimeException("did not match an edge label")
        }
        case _ => throw new RuntimeException("did not match an edge")
      }
    }

    def nodeTransformer(innerNode: Graph[NodeLabel[K],LkDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
      Some((root, DotNodeStmt(NodeId(innerNode.value.toString), Seq.empty[DotAttr])))

    graph.toDot(root, edgeTransformer, iNodeTransformer=Some(nodeTransformer))
  }

  def edgeColor(module: Module[_,_], sizeLeft: Int, sizeRight: Int): String = module match {
    case _: TypeAA[_] => Seq("black", "blue", "red", "green", "purple")((sizeLeft + sizeRight) min 4)
    case _: TypeDA[_] => Seq("black", "blue", "red", "green", "purple")(sizeRight min 4)
    case _: TypeDD[_] => "black"
  }
}
