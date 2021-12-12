package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{Element, Generator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

class TypeAA(ring: Z2PolynomialRing,
             leftAlgebra: AMinus,
             rightAlgebra: AMinus,
             leftScalarAction: Z2PolynomialRing.Morphism,
             rightScalarAction: Z2PolynomialRing.Morphism)
            (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
             rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
             graph: Graph[Generator[TypeAA], LkDiEdge] = Graph.empty) extends Module[TypeAA](
  ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(leftTensorAlgebra, rightTensorAlgebra, graph) {
  override def self: TypeAA = this

  override def companion: ModuleCompanion[TypeAA] = TypeAA

  override def buildModule(ring: Z2PolynomialRing, leftAlgebra: AMinus, rightAlgebra: AMinus,
                           leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)(
    leftTensorAlgebra: TensorAlgebra, rightTensorAlgebra: TensorAlgebra, graph: Graph[Generator[TypeAA], LkDiEdge]): TypeAA =
    new TypeAA(ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph)
}

object TypeAA extends ModuleCompanion[TypeAA] {
  def getLeftGenerator(source: Module.TensorGenerator[TypeAA],
                       target: Module.TensorGenerator[TypeAA]): TensorAlgebra.Generator = source.left

  def getRightGenerator(source: Module.TensorGenerator[TypeAA],
                        target: Module.TensorGenerator[TypeAA]): TensorAlgebra.Generator = source.right

  override def isIdempotentAction(left: TensorAlgebra.Generator,
                                  coefficient: Z2PolynomialRing.Element,
                                  right: TensorAlgebra.Generator): Boolean =
    (left.factors.length == 1) && (left.factors(0).isIdempotent) && (right.factors.isEmpty) ||
      (left.factors.isEmpty) && (right.factors.length == 1) && (right.factors(0).isIdempotent)


  implicit class AMinusExtensions(a: AMinus) {
    def asTypeAA: TypeAA = {
      var result = new TypeAA(a.ring, a, a,
        Z2PolynomialRing.Morphism.identity(a.ring), Z2PolynomialRing.Morphism.identity(a.ring))()
      for (g <- a.gens) {
        result = result.addGenerator(new Generator[TypeAA](result, g, g.leftIdempotent, g.rightIdempotent))
      }
      for (g <- a.gens) {
        result = result.addStructureMap(g.asTypeAAGenerator(result), g.d.asTypeAAElement(result))
        for (h <- a.gens) {
          if (h.rightIdempotent == g.leftIdempotent) {
            result =
              result.addStructureMap((h <*>: g.asTypeAAGenerator(result)).forceGen, (h * g).asTypeAAElement(result))
          }
          if (g.rightIdempotent == h.leftIdempotent) {
            result =
              result.addStructureMap((g.asTypeAAGenerator(result) :<*> h).forceGen, (g * h).asTypeAAElement(result))
          }
        }
      }
      result
    }
  }

  implicit class AMinusGeneratorExtensions(g: AMinus.Generator) {
    def asTypeAAGenerator(module: TypeAA): Generator[TypeAA] =
      new Generator[TypeAA](module, g, g.leftIdempotent, g.rightIdempotent)
  }

  implicit class AMinusElementExtensions(e: AMinus.Element) {
    def asTypeAAElement(module: TypeAA): Element[TypeAA] = {
      var result = module.zero
      for ((g, c) <- e.terms) {
        result += c *: g.asTypeAAGenerator(module).toElement
      }
      return result
    }
  }
}