package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.NodeLabel
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeAD[K](ring: Z2PolynomialRing,
                leftAlgebra: AMinus,
                rightAlgebra: AMinus,
                leftScalarAction: Z2PolynomialRing.Morphism,
                rightScalarAction: Z2PolynomialRing.Morphism)
               (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                graph: Graph[NodeLabel[K], LkDiEdge] = Graph.empty[NodeLabel[K], LkDiEdge]) extends Module[TypeAD[K], K](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeAD[K] = this

  override def companion: ModuleCompanion = TypeAD

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
                            leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[NodeLabel[K], LkDiEdge]): TypeAD[K] =
    new TypeAD[K](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeAD extends ModuleCompanion {
  def getLeftGenerator[M <: Module[M, K],K](source: Module.TensorGenerator[M,K],
                                            target: Module.TensorGenerator[M,K]): TensorAlgebra.Generator = source.left

  def getRightGenerator[M <: Module[M, K],K](source: Module.TensorGenerator[M,K],
                                             target: Module.TensorGenerator[M,K]): TensorAlgebra.Generator = target.right

  override def isIdempotentAction[M <: Module[M, K],K](left: TensorAlgebra.Generator,
                                                       coefficient: Z2PolynomialRing.Element,
                                                       right: TensorAlgebra.Generator): Boolean =
    (left.factors.length == 1) && left.factors(0).isIdempotent && right.factors(0).isIdempotent

  override def isValidStructureMap[M <: Module[M, K], K](source: Module.TensorGenerator[M, K],
                                                         target: Module.TensorElement[M, K]): Boolean =
    source.right.factors.isEmpty &&
      target.terms.keys.forall(t => (t.right.factors.length == 1) && t.left.factors.isEmpty)
}