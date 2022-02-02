package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.Generator
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeAD[L](ring: Z2PolynomialRing,
                leftAlgebra: AMinus,
                rightAlgebra: AMinus,
                leftScalarAction: Z2PolynomialRing.Morphism,
                rightScalarAction: Z2PolynomialRing.Morphism)
               (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                graph: Graph[Generator[TypeAD[L], L], LkDiEdge] = Graph.empty[Generator[TypeAD[L], L], LkDiEdge]) extends Module[TypeAD[L], L](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeAD[L] = this

  override def companion: ModuleCompanion = TypeAD

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
                            leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[Generator[TypeAD[L],L], LkDiEdge]): TypeAD[L] =
    new TypeAD[L](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeAD extends ModuleCompanion {
  def getLeftGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                                            target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = source.left

  def getRightGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                                             target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = target.right

  override def isIdempotentAction[M <: Module[M, L],L](left: TensorAlgebra.Generator,
                                                       coefficient: Z2PolynomialRing.Element,
                                                       right: TensorAlgebra.Generator): Boolean =
    (left.factors.length == 1) && (left.factors(0).isIdempotent) && (right.factors(0).isIdempotent)

  override def isValidStructureMap[M <: Module[M, L], L](source: Module.TensorGenerator[M, L],
                                                         target: Module.TensorElement[M, L]): Boolean =
    source.right.factors.isEmpty &&
      target.terms.keys.forall(t => (t.right.factors.length == 1) && t.left.factors.isEmpty)
}