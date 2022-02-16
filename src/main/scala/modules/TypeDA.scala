package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.Generator
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeDA[L](ring: Z2PolynomialRing,
                leftAlgebra: AMinus,
                rightAlgebra: AMinus,
                leftScalarAction: Z2PolynomialRing.Morphism,
                rightScalarAction: Z2PolynomialRing.Morphism)
               (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                graph: Graph[Generator[TypeDA[L], L], LkDiEdge] = Graph.empty[Generator[TypeDA[L], L], LkDiEdge]) extends Module[TypeDA[L], L](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeDA[L] = this

  override def companion: ModuleCompanion = TypeDA

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
                            leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[Generator[TypeDA[L],L], LkDiEdge]): TypeDA[L] =
    new TypeDA[L](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeDA extends ModuleCompanion {
  def getLeftGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                                            target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = target.left

  def getRightGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                                             target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = source.right

  override def isIdempotentAction[M <: Module[M, L],L](left: TensorAlgebra.Generator,
                                                       coefficient: Z2PolynomialRing.Element,
                                                       right: TensorAlgebra.Generator): Boolean =
    left.factors(0).isIdempotent && (right.factors.length == 1) && right.factors(0).isIdempotent

  override def isValidStructureMap[M <: Module[M, L], L](source: Module.TensorGenerator[M, L],
                                                         target: Module.TensorElement[M, L]): Boolean =
    source.left.factors.isEmpty &&
      target.terms.keys.forall(t => (t.left.factors.length == 1) && t.right.factors.isEmpty)
}