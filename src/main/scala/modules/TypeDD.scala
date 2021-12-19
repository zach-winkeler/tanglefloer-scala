package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{Element, Generator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeDD[L](ring: Z2PolynomialRing,
                leftAlgebra: AMinus,
                rightAlgebra: AMinus,
                leftScalarAction: Z2PolynomialRing.Morphism,
                rightScalarAction: Z2PolynomialRing.Morphism)
               (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                graph: Graph[Generator[TypeDD[L], L], LkDiEdge] = Graph.empty[Generator[TypeDD[L], L], LkDiEdge]) extends Module[TypeDD[L], L](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeDD[L] = this

  override def companion: ModuleCompanion = TypeDD

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
                            leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[Generator[TypeDD[L],L], LkDiEdge]): TypeDD[L] =
    new TypeDD[L](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeDD extends ModuleCompanion {
  def getLeftGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                                            target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = target.left

  def getRightGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                                             target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = target.right

  override def isIdempotentAction[M <: Module[M, L],L](left: TensorAlgebra.Generator,
                                                       coefficient: Z2PolynomialRing.Element,
                                                       right: TensorAlgebra.Generator): Boolean = false

  override def isValidStructureMap[M <: Module[M, L], L](source: Module.TensorGenerator[M, L],
                                                         target: Module.TensorElement[M, L]): Boolean =
    source.left.factors.isEmpty && source.right.factors.isEmpty
}