package modules

import algebras.TensorAlgebra._
import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import cats.implicits._
import modules.Module.{NodeLabel, TensorElement, TensorGenerator}
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.Graph

abstract class Module[M <: Module[M, K], K]
  (val ring: Z2PolynomialRing,
   val leftAlgebra: AMinus,
   val rightAlgebra: AMinus,
   val leftScalarAction: Z2PolynomialRing.Morphism,
   val rightScalarAction: Z2PolynomialRing.Morphism)
  (val leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
   val rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
   var graph: Graph[NodeLabel[K], LkDiEdge] = Graph.empty[NodeLabel[K], LkDiEdge]) {

  import Module._

  def self: M

  def companion: ModuleCompanion

  def buildModule(ring: Z2PolynomialRing,
                  leftAlgebra: AMinus, rightAlgebra: AMinus,
                  leftScalarAction: Z2PolynomialRing.Morphism, rightScalarAction: Z2PolynomialRing.Morphism)
                 (leftTensorAlgebra: TensorAlgebra = new TensorAlgebra(leftAlgebra),
                  rightTensorAlgebra: TensorAlgebra = new TensorAlgebra(rightAlgebra),
                  graph: Graph[NodeLabel[K], LkDiEdge] = Graph.empty): M

  def zero: Element[M, K] = new Element(this, Map.empty)

  def addGenerator(label: NodeLabel[K]): Unit = {
    graph = graph + label
  }

  def gen(label: NodeLabel[K]): Generator[M, K] = {
    assert((graph find label).nonEmpty)
    new Generator(this, label.key, label.leftIdempotent, label.rightIdempotent)
  }

  def gens: Set[Generator[M, K]] =
    graph.nodes.view.map((innerNode: Graph[NodeLabel[K], LkDiEdge]#NodeT) => innerNode.value).toSet.map(gen)

  def addStructureMap(input: TensorGenerator[M, K], output: TensorElement[M, K]): Unit = {
    assert(companion.isValidStructureMap(input, output))
    for ((g, c) <- output.terms) {
      addArrow(input, g, c)
    }
  }

  def addArrow(source: TensorGenerator[M, K], target: TensorGenerator[M, K], coefficient: Z2PolynomialRing.Element): Unit = {
    val left = companion.getLeftGenerator(source, target)
    val right = companion.getRightGenerator(source, target)
    val s = source.label
    val t = target.label
    if (!((graph find s).nonEmpty && (graph find t).nonEmpty)) {
      print("hi")
    }
    assert((graph find s).nonEmpty && (graph find t).nonEmpty)
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

  def summands: Seq[M] =
    this.graph.componentTraverser().map(comp =>
      buildModule(ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)(
        leftTensorAlgebra, rightTensorAlgebra, comp.to(Graph))).toSeq

  def canEqual(other: Any): Boolean = other.isInstanceOf[Module[M, K]]

  override def equals(other: Any): Boolean = other match {
    case that: Module[M, K] =>
      (that canEqual this) &&
        ring == that.ring &&
        leftAlgebra == that.leftAlgebra &&
        rightAlgebra == that.rightAlgebra &&
        leftScalarAction == that.leftScalarAction &&
        rightScalarAction == that.rightScalarAction &&
        leftTensorAlgebra == that.leftTensorAlgebra &&
        rightTensorAlgebra == that.rightTensorAlgebra &&
        graph == that.graph
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction,
      leftTensorAlgebra, rightTensorAlgebra, graph)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

trait ModuleCompanion {
  def getLeftGenerator[M <: Module[M, K], K](source: TensorGenerator[M, K], target: TensorGenerator[M, K]): TensorAlgebra.Generator

  def getRightGenerator[M <: Module[M, K], K](source: TensorGenerator[M, K], target: TensorGenerator[M, K]): TensorAlgebra.Generator

  def isIdempotentAction[M <: Module[M, K], K](left: TensorAlgebra.Generator,
                                               coefficient: Z2PolynomialRing.Element,
                                               right: TensorAlgebra.Generator): Boolean

  def isValidStructureMap[M <: Module[M, K], K](source: TensorGenerator[M, K], target: TensorElement[M, K]): Boolean
}

object Module {
  def directSum[M <: Module[M, K], K](summands: Seq[Module[M, K]]): M =
    if (summands.isEmpty) {
      throw new RuntimeException("empty direct sum not implemented")
    } else {
      val m = summands.head
      m.buildModule(m.ring, m.leftAlgebra, m.rightAlgebra, m.leftScalarAction, m.rightScalarAction)(
        m.leftTensorAlgebra, m.rightTensorAlgebra, summands.view.map(_.graph).fold(Graph.empty) {_.union(_)})
    }

  class TensorGenerator[M <: Module[M, K], K](val module: Module[M, K], val key: K, val leftIdempotent: AMinus.Generator, val rightIdempotent: AMinus.Generator, val left: TensorAlgebra.Generator, val right: TensorAlgebra.Generator) {
    def label: NodeLabel[K] =
      NodeLabel(key,
      if (left.factors.isEmpty) leftIdempotent else left.factors.last.rightIdempotent,
        if (right.factors.isEmpty) rightIdempotent else right.factors.head.leftIdempotent)

    def toElement: TensorElement[M, K] = new TensorElement(module, Map(this -> module.ring.one))

    def getGenerator: Generator[M, K] = new Generator(module, key, leftIdempotent, rightIdempotent)

    def <*>:(g: TensorAlgebra.Generator): TensorElement[M, K] = {
      if (g.rightIdempotent.isEmpty) {
        this.toElement
      } else if (g.rightIdempotent.contains(this.leftIdempotent)) {
        new TensorGenerator[M, K](module, key, g.leftIdempotent.get, this.rightIdempotent, (g <*> left).forceGen, right).toElement
      } else {
        module.zero
      }
    }

    def <*>:(g: AMinus.Generator): TensorElement[M, K] = {
      g.toTensorAlgebra(module.leftTensorAlgebra) <*>: this
    }

    def :<*>(g: TensorAlgebra.Generator): TensorElement[M, K] = {
      if (g.leftIdempotent.isEmpty) {
        this.toElement
      } else if (g.leftIdempotent.contains(this.rightIdempotent)) {
        new TensorGenerator[M, K](module, key, this.leftIdempotent, g.rightIdempotent.get, left, (right <*> g).forceGen).toElement
      } else {
        module.zero
      }
    }

    def :<*>(g: AMinus.Generator): TensorElement[M, K] = {
      this :<*> g.toTensorAlgebra(module.rightTensorAlgebra)
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[TensorGenerator[M, K]]

    override def equals(other: Any): Boolean = other match {
      case that: TensorGenerator[M, K] =>
        (that canEqual this) &&
          module == that.module &&
          key == that.key &&
          leftIdempotent == that.leftIdempotent &&
          rightIdempotent == that.rightIdempotent &&
          left == that.left &&
          right == that.right
      case _ => false
    }

    override def hashCode: Int = {
      val state = Seq(key, leftIdempotent, rightIdempotent, left, right)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString: String = left.toString + key.toString + right.toString
  }

  class Generator[M <: Module[M, K], K](module: Module[M, K], key: K, leftIdempotent: AMinus.Generator, rightIdempotent: AMinus.Generator)
    extends TensorGenerator[M, K](module, key, leftIdempotent, rightIdempotent, module.leftTensorAlgebra.oneGen, module.rightTensorAlgebra.oneGen) {
    override def toElement: Element[M, K] = new Element(module, Map(this -> module.ring.one))

    override def equals(other: Any): Boolean = other match {
      case that: Generator[M, K] => this.key == that.key
      case _ => false
    }

    def incoming: Set[LkDiEdge[Graph[NodeLabel[K], LkDiEdge]#NodeT]] = (module.graph get label).incoming.map(_.edge)

    def outgoing: Set[LkDiEdge[Graph[NodeLabel[K], LkDiEdge]#NodeT]] = (module.graph get label).outgoing.map(_.edge)

    override def hashCode: Int = Seq(key).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = key.toString
  }

  class TensorElement[M <: Module[M, K], K](val module: Module[M, K],
                                            _terms: Map[_ <: TensorGenerator[M, K], Z2PolynomialRing.Element]) {
    val terms: Map[_ <: TensorGenerator[M, K], Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)

    def +(other: TensorElement[M, K]): TensorElement[M, K] = {
      assert(this.module == other.module)
      new TensorElement(this.module,
        this.terms.asInstanceOf[Map[TensorGenerator[M, K], Z2PolynomialRing.Element]]
          |+| other.terms.asInstanceOf[Map[TensorGenerator[M, K], Z2PolynomialRing.Element]])
    }

    def *:(scalar: Z2PolynomialRing.Element): TensorElement[M, K] =
      new TensorElement(module, terms.view.mapValues(scalar * _).toMap)

    def <*>:(other: TensorAlgebra.Element): TensorElement[M, K] = {
      var result: TensorElement[M, K] = module.zero
      for ((g1, c1) <- other.terms; (g2, c2) <- this.terms) {
        result += (module.leftScalarAction(c1) * c2) *: (g1 <*>: g2)
      }
      result
    }

    def <*>:(other: AMinus.Element): TensorElement[M, K] = {
      other.toTensorAlgebra(module.leftTensorAlgebra) <*>: this
    }

    def :<*>(other: TensorAlgebra.Element): TensorElement[M, K] = {
      var result: TensorElement[M, K] = module.zero
      for ((g1, c1) <- this.terms; (g2, c2) <- other.terms) {
        result += (c1 * module.rightScalarAction(c2)) *: (g1 :<*> g2)
      }
      result
    }

    def :<*>(other: AMinus.Element): TensorElement[M, K] = {
      this :<*> other.toTensorAlgebra(module.rightTensorAlgebra)
    }

    def forceGen: TensorGenerator[M, K] = {
      assert(terms.tail.isEmpty && (terms.head._2 == module.ring.one))
      terms.head._1
    }

    override def toString: String = terms.mkString(" + ") match {
      case "" => "0"
      case s => s
    }
  }

  class Element[M <: Module[M, K], K](module: Module[M, K], _terms: Map[Generator[M, K], Z2PolynomialRing.Element])
    extends TensorElement[M, K](module, _terms) {
    override val terms: Map[Generator[M, K], Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)

    def +(other: Element[M, K]): Element[M, K] = {
      assert(this.module == other.module)
      new Element(this.module, this.terms |+| other.terms)
    }

    override def *:(scalar: Z2PolynomialRing.Element): Element[M, K] =
      new Element(module, terms.view.mapValues(scalar * _).toMap)

    override def forceGen: Generator[M, K] = {
      assert(terms.tail.isEmpty && (terms.head._2 == module.ring.one))
      terms.head._1
    }
  }

  case class NodeLabel[K](key: K, leftIdempotent: AMinus.Generator, rightIdempotent: AMinus.Generator) {
    override def toString: String = key.toString.replaceAll("Set","")
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
