package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{Element, Generator, TensorElement, TensorGenerator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeAA[L](ring: Z2PolynomialRing,
             leftAlgebra: AMinus,
             rightAlgebra: AMinus,
             leftScalarAction: Z2PolynomialRing.Morphism,
             rightScalarAction: Z2PolynomialRing.Morphism)
            (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
             rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
             graph: Graph[Generator[TypeAA[L], L], LkDiEdge] = Graph.empty[Generator[TypeAA[L], L], LkDiEdge]) extends Module[TypeAA[L], L](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeAA[L] = this

  override def companion: ModuleCompanion = TypeAA

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
    leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[Generator[TypeAA[L],L], LkDiEdge]): TypeAA[L] =
    new TypeAA[L](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeAA extends ModuleCompanion {
  def getLeftGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                       target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = source.left

  def getRightGenerator[M <: Module[M, L],L](source: Module.TensorGenerator[M,L],
                        target: Module.TensorGenerator[M,L]): TensorAlgebra.Generator = source.right

  override def isIdempotentAction[M <: Module[M, L],L](left: TensorAlgebra.Generator,
                                  coefficient: Z2PolynomialRing.Element,
                                  right: TensorAlgebra.Generator): Boolean =
    (left.factors.length == 1) && left.factors(0).isIdempotent && right.factors.isEmpty ||
      left.factors.isEmpty && (right.factors.length == 1) && right.factors(0).isIdempotent

  override def isValidStructureMap[M <: Module[M, L], L](source: TensorGenerator[M, L],
                                                         target: TensorElement[M, L]): Boolean =
    target.terms.keys.forall(t => t.left.factors.isEmpty && t.right.factors.isEmpty)

  implicit class AMinusExtensions(a: AMinus) {
    def asTypeAA: TypeAA[AMinus.Generator] = {
      var result = new TypeAA[AMinus.Generator](a.ring, a, a,
        Z2PolynomialRing.Morphism.identity(a.ring), Z2PolynomialRing.Morphism.identity(a.ring))()
      for (g <- a.gens) {
        result.addGenerator(new Generator[TypeAA[AMinus.Generator],AMinus.Generator](result, g, g.leftIdempotent, g.rightIdempotent))
      }
      for (g <- a.gens) {
        result.addStructureMap(g.asTypeAAGenerator(result), g.d.asTypeAAElement(result))
        for (h <- a.gens) {
          if (h.rightIdempotent == g.leftIdempotent) {
            result.addStructureMap((h <*>: g.asTypeAAGenerator(result)).forceGen, (h * g).asTypeAAElement(result))
          }
          if (g.rightIdempotent == h.leftIdempotent) {
            result.addStructureMap((g.asTypeAAGenerator(result) :<*> h).forceGen, (g * h).asTypeAAElement(result))
          }
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