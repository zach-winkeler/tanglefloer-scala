package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import algebras.TensorAlgebra._
import modules.Module.{Generator, TensorElement, TensorGenerator}
import scalax.collection.edge.LkDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.immutable.Graph
import cats.implicits._

abstract class Module[M <: Module[M, L], L]
  (val ring: Z2PolynomialRing,
   val leftAlgebra: AMinus,
   val rightAlgebra: AMinus,
   val leftScalarAction: Z2PolynomialRing.Morphism,
   val rightScalarAction: Z2PolynomialRing.Morphism)
  (val leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
   val rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
   var graph: Graph[Generator[M, L], LkDiEdge] = Graph.empty[Generator[M, L], LkDiEdge]) {

  import Module._

  def self: M

  def companion: ModuleCompanion

  def buildModule(ring: Z2PolynomialRing,
                  leftAlgebra: AMinus, rightAlgebra: AMinus,
                  leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)
                 (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                  rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                  graph: Graph[Generator[M, L], LkDiEdge] = Graph.empty): M

  def zero: Element[M, L] = new Element(this, Map.empty)

  def addGenerator(g: Generator[M, L]): Unit = {
    graph = graph + g
  }

  def gens: Set[Generator[M, L]] =
    graph.nodes.view.map((innerNode: Graph[Generator[M, L], LkDiEdge]#NodeT) => innerNode.value).toSet

  def addStructureMap(input: TensorGenerator[M, L], output: TensorElement[M, L]): Unit = {
    assert(companion.isValidStructureMap(input, output))
    for ((g, c) <- output.terms) {
      addArrow(input, g, c)
    }
  }

  def addArrow(source: TensorGenerator[M, L], target: TensorGenerator[M, L], coefficient: Z2PolynomialRing.Element): Unit = {
    val left = companion.getLeftGenerator(source, target)
    val right = companion.getRightGenerator(source, target)
    val s = source.getGenerator
    val t = target.getGenerator
    val oldEdge = (s ~+#> t) (EdgeLabel(left, coefficient, right))
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
      newGraph = newGraph incl (s ~+#> t) (EdgeLabel(left, newCoefficient, right))
    }
    graph = newGraph
  }
}

trait ModuleCompanion {
  def getLeftGenerator[M <: Module[M, L], L](source: TensorGenerator[M, L], target: TensorGenerator[M, L]): TensorAlgebra.Generator

  def getRightGenerator[M <: Module[M, L], L](source: TensorGenerator[M, L], target: TensorGenerator[M, L]): TensorAlgebra.Generator

  def isIdempotentAction[M <: Module[M, L], L](left: TensorAlgebra.Generator,
                                               coefficient: Z2PolynomialRing.Element,
                                               right: TensorAlgebra.Generator): Boolean

  def isValidStructureMap[M <: Module[M, L], L](source: TensorGenerator[M, L], target: TensorElement[M, L]): Boolean
}

object Module {
  class TensorGenerator[M <: Module[M, L], L](val module: Module[M, L], val label: L, val leftIdempotent: AMinus.Generator, val rightIdempotent: AMinus.Generator, val left: TensorAlgebra.Generator, val right: TensorAlgebra.Generator) {
    def toElement: TensorElement[M, L] = new TensorElement(module, Map(this -> module.ring.one))

    def getGenerator: Generator[M, L] = new Generator(module, label, leftIdempotent, rightIdempotent)

    def <*>:(g: TensorAlgebra.Generator): TensorElement[M, L] = {
      if (g.rightIdempotent.isEmpty) {
        this.toElement
      } else if (g.rightIdempotent.contains(this.leftIdempotent)) {
        new TensorGenerator[M, L](module, label, g.leftIdempotent.get, this.rightIdempotent, (g <*> left).forceGen, right).toElement
      } else {
        module.zero
      }
    }

    def <*>:(g: AMinus.Generator): TensorElement[M, L] = {
      g.toTensorAlgebra(module.leftTensorAlgebra) <*>: this
    }

    def :<*>(g: TensorAlgebra.Generator): TensorElement[M, L] = {
      if (g.leftIdempotent.isEmpty) {
        this.toElement
      } else if (g.leftIdempotent.contains(this.rightIdempotent)) {
        new TensorGenerator[M, L](module, label, this.leftIdempotent, g.rightIdempotent.get, left, (right <*> g).forceGen).toElement
      } else {
        module.zero
      }
    }

    def :<*>(g: AMinus.Generator): TensorElement[M, L] = {
      this :<*> g.toTensorAlgebra(module.rightTensorAlgebra)
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[TensorGenerator[M, L]]

    override def equals(other: Any): Boolean = other match {
      case that: TensorGenerator[M, L] =>
        (that canEqual this) &&
          module == that.module &&
          label == that.label &&
          leftIdempotent == that.leftIdempotent &&
          rightIdempotent == that.rightIdempotent &&
          left == that.left &&
          right == that.right
      case _ => false
    }

    override def hashCode: Int = {
      val state = Seq(label, leftIdempotent, rightIdempotent, left, right)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString: String = left.toString + label.toString + right.toString
  }

  class Generator[M <: Module[M, L], L](module: Module[M, L], label: L, leftIdempotent: AMinus.Generator, rightIdempotent: AMinus.Generator)
    extends TensorGenerator[M, L](module, label, leftIdempotent, rightIdempotent, module.leftTensorAlgebra.oneGen, module.rightTensorAlgebra.oneGen) {
    override def toElement: Element[M, L] = new Element(module, Map(this -> module.ring.one))

    override def equals(other: Any): Boolean = other match {
      case that: Generator[M, L] => this.label == that.label
      case _ => false
    }

    def incoming: Set[LkDiEdge[Graph[Generator[M, L], LkDiEdge]#NodeT]] = (module.graph get this).incoming.map(_.edge)

    def outgoing: Set[LkDiEdge[Graph[Generator[M, L], LkDiEdge]#NodeT]] = (module.graph get this).outgoing.map(_.edge)

    override def hashCode: Int = Seq(label).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = label.toString
  }

  class TensorElement[M <: Module[M, L], L](val module: Module[M, L],
                                            _terms: Map[_ <: TensorGenerator[M, L], Z2PolynomialRing.Element]) {
    val terms: Map[_ <: TensorGenerator[M, L], Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)

    def +(other: TensorElement[M, L]): TensorElement[M, L] = {
      assert(this.module == other.module)
      new TensorElement(this.module,
        this.terms.asInstanceOf[Map[TensorGenerator[M, L], Z2PolynomialRing.Element]]
          |+| other.terms.asInstanceOf[Map[TensorGenerator[M, L], Z2PolynomialRing.Element]])
    }

    def *:(scalar: Z2PolynomialRing.Element): TensorElement[M, L] =
      new TensorElement(module, terms.view.mapValues(scalar * _).toMap)

    def <*>:(other: TensorAlgebra.Element): TensorElement[M, L] = {
      var result: TensorElement[M, L] = module.zero
      for ((g1, c1) <- other.terms; (g2, c2) <- this.terms) {
        result += (module.leftScalarAction(c1) * c2) *: (g1 <*>: g2)
      }
      result
    }

    def <*>:(other: AMinus.Element): TensorElement[M, L] = {
      other.toTensorAlgebra(module.leftTensorAlgebra) <*>: this
    }

    def :<*>(other: TensorAlgebra.Element): TensorElement[M, L] = {
      var result: TensorElement[M, L] = module.zero
      for ((g1, c1) <- this.terms; (g2, c2) <- other.terms) {
        result += (c1 * module.rightScalarAction(c2)) *: (g1 :<*> g2)
      }
      result
    }

    def :<*>(other: AMinus.Element): TensorElement[M, L] = {
      this :<*> other.toTensorAlgebra(module.rightTensorAlgebra)
    }

    def forceGen: TensorGenerator[M, L] = {
      assert(terms.tail.isEmpty && (terms.head._2 == module.ring.one))
      terms.head._1
    }

    override def toString: String = terms.mkString(" + ") match {
      case "" => "0"
      case s => s
    }
  }

  class Element[M <: Module[M, L], L](module: Module[M, L], _terms: Map[Generator[M, L], Z2PolynomialRing.Element])
    extends TensorElement[M, L](module, _terms) {
    override val terms: Map[Generator[M, L], Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)

    def +(other: Element[M, L]): Element[M, L] = {
      assert(this.module == other.module)
      new Element(this.module, this.terms |+| other.terms)
    }

    override def *:(scalar: Z2PolynomialRing.Element): Element[M, L] =
      new Element(module, terms.view.mapValues(scalar * _).toMap)

    override def forceGen: Generator[M, L] = {
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
