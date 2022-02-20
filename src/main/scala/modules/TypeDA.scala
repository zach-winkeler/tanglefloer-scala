package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.NodeLabel
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeDA[K](ring: Z2PolynomialRing,
                leftAlgebra: AMinus,
                rightAlgebra: AMinus,
                leftScalarAction: Z2PolynomialRing.Morphism,
                rightScalarAction: Z2PolynomialRing.Morphism)
               (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                graph: Graph[NodeLabel[K], LkDiEdge] = Graph.empty[NodeLabel[K], LkDiEdge]) extends Module[TypeDA[K], K](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeDA[K] = this

  override def companion: ModuleCompanion = TypeDA

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
                            leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[NodeLabel[K], LkDiEdge]): TypeDA[K] =
    new TypeDA[K](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeDA extends ModuleCompanion {
  def getLeftGenerator[M <: Module[M, K],K](source: Module.TensorGenerator[M,K],
                                            target: Module.TensorGenerator[M,K]): TensorAlgebra.Generator = target.left

  def getRightGenerator[M <: Module[M, K],K](source: Module.TensorGenerator[M,K],
                                             target: Module.TensorGenerator[M,K]): TensorAlgebra.Generator = source.right

  override def isIdempotentAction[M <: Module[M, K],K](left: TensorAlgebra.Generator,
                                                       coefficient: Z2PolynomialRing.Element,
                                                       right: TensorAlgebra.Generator): Boolean =
    left.factors(0).isIdempotent && (right.factors.length == 1) && right.factors(0).isIdempotent

  override def isValidStructureMap[M <: Module[M, K], K](source: Module.TensorGenerator[M, K],
                                                         target: Module.TensorElement[M, K]): Boolean =
    source.left.factors.isEmpty &&
      target.terms.keys.forall(t => (t.left.factors.length == 1) && t.right.factors.isEmpty)
}