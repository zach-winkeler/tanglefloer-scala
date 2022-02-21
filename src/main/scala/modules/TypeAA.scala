package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{Element, Generator, NodeLabel, TensorElement, TensorGenerator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeAA[K](ring: Z2PolynomialRing,
             leftAlgebra: AMinus,
             rightAlgebra: AMinus,
             leftScalarAction: Z2PolynomialRing.Morphism,
             rightScalarAction: Z2PolynomialRing.Morphism)
            (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
             rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
             graph: Graph[NodeLabel[K], LkDiEdge] = Graph.empty[NodeLabel[K], LkDiEdge]) extends Module[TypeAA[K], K](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeAA[K] = this

  override def companion: ModuleCompanion = TypeAA

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
    leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[NodeLabel[K], LkDiEdge]): TypeAA[K] =
    new TypeAA[K](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeAA extends ModuleCompanion {
  def getLeftGenerator[M <: Module[M, K],K](source: Module.TensorGenerator[M,K],
                       target: Module.TensorGenerator[M,K]): TensorAlgebra.Generator = source.left

  def getRightGenerator[M <: Module[M, K],K](source: Module.TensorGenerator[M,K],
                        target: Module.TensorGenerator[M,K]): TensorAlgebra.Generator = source.right

  override def isIdempotentAction[M <: Module[M, K],K](left: TensorAlgebra.Generator,
                                  coefficient: Z2PolynomialRing.Element,
                                  right: TensorAlgebra.Generator): Boolean =
    (left.factors.length == 1) && left.factors(0).isIdempotent && right.factors.isEmpty ||
      left.factors.isEmpty && (right.factors.length == 1) && right.factors(0).isIdempotent

  override def isValidStructureMap[M <: Module[M, K], K](source: TensorGenerator[M, K],
                                                         target: TensorElement[M, K]): Boolean =
    target.terms.keys.forall(t => t.left.factors.isEmpty && t.right.factors.isEmpty)

  implicit class AMinusExtensions(a: AMinus) {
    def asTypeAA: TypeAA[AMinus.Generator] = {
      val result = new TypeAA[AMinus.Generator](a.ring, a, a,
        Z2PolynomialRing.Morphism.identity(a.ring), Z2PolynomialRing.Morphism.identity(a.ring))()
      for (g <- a.gens()) {
        result.addGenerator(NodeLabel(g, g.leftIdempotent, g.rightIdempotent))
      }
      for (g <- a.gens()) {
        result.addStructureMap(g.asTypeAAGenerator(result), g.d.asTypeAAElement(result))
        for (h <- a.gens(rightIdempotent = Some(g.leftIdempotent))) {
          result.addStructureMap((h <*>: g.asTypeAAGenerator(result)).forceGen, (h * g).asTypeAAElement(result))
        }
        for (h <- a.gens(leftIdempotent = Some(g.rightIdempotent))) {
          result.addStructureMap((g.asTypeAAGenerator(result) :<*> h).forceGen, (g * h).asTypeAAElement(result))
        }
      }
      result
    }
  }

  implicit class AMinusGeneratorExtensions(g: AMinus.Generator) {
    def asTypeAAGenerator(module: TypeAA[AMinus.Generator]): Generator[TypeAA[AMinus.Generator],AMinus.Generator] =
      new Generator[TypeAA[AMinus.Generator],AMinus.Generator](module, g, g.leftIdempotent, g.rightIdempotent)
  }

  implicit class AMinusElementExtensions(e: AMinus.Element) {
    def asTypeAAElement(module: TypeAA[AMinus.Generator]): Element[TypeAA[AMinus.Generator],AMinus.Generator] = {
      var result = module.zero
      for ((g, c) <- e.terms) {
        result += c *: g.asTypeAAGenerator(module).toElement
      }
      result
    }
  }
}