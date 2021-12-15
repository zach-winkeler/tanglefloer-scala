package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{Generator, TensorGenerator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.immutable.Graph
import scalaz.Scalaz._

abstract class Module[M <: Module[M]](val ring: Z2PolynomialRing,
             val leftAlgebra: AMinus,
             val rightAlgebra: AMinus,
             val leftScalarAction: Z2PolynomialRing.Morphism,
             val rightScalarAction: Z2PolynomialRing.Morphism)
            (val leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
             val rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
             val graph: Graph[Generator[M], LkDiEdge] = Graph.empty[Generator[M], LkDiEdge]) {
  import Module._

  def self: M

  def companion: ModuleCompanion[M]

  def buildModule(ring: Z2PolynomialRing,
                  leftAlgebra: AMinus, rightAlgebra: AMinus,
                  leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)
                 (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                  rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                  graph: Graph[Generator[M], LkDiEdge] = Graph.empty): M

  def zero: Element[M] = new Element(this, Map.empty)

  def addGenerator(g: Generator[M]): M = {
    buildModule(
      ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, graph + g)
  }

  def addStructureMap(input: TensorGenerator[M], output: TensorElement[M]): M = {
    var result: M = self
    for ((g, c) <- output.terms) {
      result = result.addArrow(input, g, c)
    }
    result
  }

  def addArrow(source: TensorGenerator[M], target: TensorGenerator[M], coefficient: Z2PolynomialRing.Element): M = {
    val left = companion.getLeftGenerator(source, target)
    val right = companion.getRightGenerator(source, target)
    val s = source.getGenerator
    val t = target.getGenerator
    val oldEdge = (s ~+#> t)(EdgeLabel(left, coefficient, right))
    val oldCoefficient = graph find oldEdge match {
      case Some(et) => et.edge match {
        case _ :~> _ + (l: EdgeLabel) => l.coefficient
        case _ => throw new RuntimeException("incorrectly labeled edge")
      }
      case None => ring.zero
    }
    val newCoefficient = oldCoefficient + coefficient
    var newGraph = graph excl oldEdge
    if (newCoefficient != ring.zero) {
      newGraph = newGraph incl (s ~+#> t)(EdgeLabel(left, newCoefficient, right))
    }
    buildModule(ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
      leftTensorAlgebra, rightTensorAlgebra, newGraph)
  }
}

trait ModuleCompanion[M <: Module[M]] {
  def getLeftGenerator(source: TensorGenerator[M], target: TensorGenerator[M]): TensorAlgebra.Generator
  def getRightGenerator(source: TensorGenerator[M], target: TensorGenerator[M]): TensorAlgebra.Generator
  def isIdempotentAction(left: TensorAlgebra.Generator,
                         coefficient: Z2PolynomialRing.Element,
                         right: TensorAlgebra.Generator): Boolean
}

object Module {
  class TensorGenerator[M <: Module[M]](val module: Module[M], val name: Object,
                                        val leftIdempotent: AMinus.Generator, val rightIdempotent: AMinus.Generator,
                                        val left: TensorAlgebra.Generator, val right: TensorAlgebra.Generator) {
    def toElement: TensorElement[M] = new TensorElement(module, Map(this -> module.ring.one))

    def getGenerator: Generator[M] = new Generator(module, name, leftIdempotent, rightIdempotent)

    def <*>:(g: AMinus.Generator): TensorElement[M] = {
      if (g.rightIdempotent == this.leftIdempotent) {
        new TensorGenerator[M](module, name, g.leftIdempotent, this.rightIdempotent,
          (g <*>: left).forceGen, right).toElement
      } else {
        module.zero
      }
    }

    def :<*>(g: AMinus.Generator): TensorElement[M] = {
      if (this.rightIdempotent == g.leftIdempotent) {
        new TensorGenerator[M](module, name, this.leftIdempotent, g.rightIdempotent,
          left, (right :<*> g).forceGen).toElement
      } else {
        module.zero
      }
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[TensorGenerator[M]]

    override def equals(other: Any): Boolean = other match {
      case that: TensorGenerator[M] =>
        (that canEqual this) &&
          module == that.module &&
          name == that.name &&
          leftIdempotent == that.leftIdempotent &&
          rightIdempotent == that.rightIdempotent &&
          left == that.left &&
          right == that.right
      case _ => false
    }

    override def hashCode: Int = {
      val state = Seq(name, leftIdempotent, rightIdempotent, left, right)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  class Generator[M <: Module[M]](module: Module[M], name: Object,
                                  leftIdempotent: AMinus.Generator, rightIdempotent: AMinus.Generator)
  extends TensorGenerator[M](
    module, name, leftIdempotent, rightIdempotent, module.leftTensorAlgebra.oneGen, module.rightTensorAlgebra.oneGen) {
    override def toElement: Element[M] = new Element(module, Map(this -> module.ring.one))

    override def equals(other: Any): Boolean = other match {
      case that: Generator[M] => this.name == that.name
      case _ => false
    }

    override def hashCode: Int = Seq(name).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = name.toString
  }

  class TensorElement[M <: Module[M]](val module: Module[M],
                                      _terms: Map[_ <: TensorGenerator[M], Z2PolynomialRing.Element]) {
    val terms: Map[_ <: TensorGenerator[M], Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)

    def +(other: TensorElement[M]): TensorElement[M] = {
      assert(this.module == other.module)
      new TensorElement(this.module,
        this.terms.asInstanceOf[Map[TensorGenerator[M], Z2PolynomialRing.Element]]
          |+| other.terms.asInstanceOf[Map[TensorGenerator[M], Z2PolynomialRing.Element]])
    }

    def *:(scalar: Z2PolynomialRing.Element): TensorElement[M] =
      new TensorElement(module, terms.view.mapValues(scalar * _).toMap)

    def forceGen: TensorGenerator[M] = {
      assert(terms.tail.isEmpty && (terms.head._2 == module.ring.one))
      terms.head._1
    }
  }

  class Element[M <: Module[M]](module: Module[M], _terms: Map[Generator[M], Z2PolynomialRing.Element])
  extends TensorElement[M](module, _terms) {
    override val terms: Map[Generator[M], Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)

    def +(other: Element[M]): Element[M] = {
      assert(this.module == other.module)
      new Element(this.module, this.terms |+| other.terms)
    }

    override def *:(scalar: Z2PolynomialRing.Element): Element[M] =
      new Element(module, terms.view.mapValues(scalar * _).toMap)

    override def forceGen: Generator[M] = {
      assert(terms.tail.isEmpty && (terms.head._2 == module.ring.one))
      terms.head._1
    }
  }

  case class EdgeLabel(left: TensorAlgebra.Generator,
                        coefficient: Z2PolynomialRing.Element,
                        right: TensorAlgebra.Generator) {
    override def equals(other: Any): Boolean = other match {
      case that: EdgeLabel => (this.left == that.left) && (this.right == that.right)
      case _ => false
    }

    override def hashCode: Int = Seq(left, right).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = (left, coefficient, right).toString
  }
}
