package algebras

import cats.Semigroup
import cats.implicits._

import utilities.MapUtils._

class TensorAlgebra(val algebra: AMinus) {
  import TensorAlgebra._
  def zero: Element = new Element(this, Map.empty)
  def one: Element = oneGen.toElement
  def oneGen: Generator = new Generator(this, IndexedSeq())

  def canEqual(other: Any): Boolean = other.isInstanceOf[TensorAlgebra]

  override def equals(other: Any): Boolean = other match {
    case that: TensorAlgebra =>
      (that canEqual this) &&
        algebra == that.algebra
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(algebra)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object TensorAlgebra {
  class Generator(val tensorAlgebra: TensorAlgebra, val factors: IndexedSeq[AMinus.Generator]) {
    def toElement: TensorAlgebra.Element = new Element(tensorAlgebra, Map(this -> tensorAlgebra.algebra.ring.one))

    def leftIdempotent: Option[AMinus.Generator] = {
      if (factors.nonEmpty) {
        Some(factors.head.leftIdempotent)
      } else {
        None
      }
    }

    def rightIdempotent: Option[AMinus.Generator] = {
      if (factors.nonEmpty) {
        Some(factors.last.rightIdempotent)
      } else {
        None
      }
    }

    def :<*>(other: AMinus.Generator): Element = this <*> other.toTensorAlgebra(tensorAlgebra)

    def <*>:(other: AMinus.Generator): Element = other.toTensorAlgebra(tensorAlgebra) <*> this

    def <*>(other: Generator): Element = {
      if (this.rightIdempotent.isEmpty || other.leftIdempotent.isEmpty || this.rightIdempotent == other.leftIdempotent) {
        new Generator(tensorAlgebra, this.factors ++ other.factors).toElement
      } else {
        tensorAlgebra.zero
      }
    }

    override def toString: String = factors.toString.substring(6)

    def canEqual(other: Any): Boolean = other.isInstanceOf[Generator]

    override def equals(other: Any): Boolean = other match {
      case that: Generator =>
        (that canEqual this) &&
          tensorAlgebra == that.tensorAlgebra &&
          factors == that.factors
      case _ => false
    }

    override def hashCode(): Int = Seq(factors).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  object Generator {

  }

  class Element(val tensorAlgebra: TensorAlgebra, val terms: Map[Generator, Z2PolynomialRing.Element]) {
    def forceGen: Generator = {
      assert(terms.tail.isEmpty && (terms.head._2 == tensorAlgebra.algebra.ring.one))
      terms.head._1
    }

    def +(other: Element): Element = new Element(this.tensorAlgebra, this.terms |+| other.terms)

    def *:(scalar: Z2PolynomialRing.Element): Element =
      new Element(tensorAlgebra, terms.view.mapValues(scalar * _).toMap)

    def :<*>(other: AMinus.Element): Element = this <*> other.toTensorAlgebra(tensorAlgebra)

    def <*>:(other: AMinus.Element): Element = other.toTensorAlgebra(tensorAlgebra) <*> this

    def <*>(other: Element): Element = {
      assert(this.tensorAlgebra == other.tensorAlgebra)
      var result = this.tensorAlgebra.zero
      for ((g1, c1) <- this.terms; (g2, c2) <- other.terms) {
        result += (c1 * c2) *: (g1 <*> g2)
      }
      result
    }

    override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Element]

    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        (that canEqual this) &&
          tensorAlgebra == that.tensorAlgebra &&
          terms == that.terms
      case _ => false
    }

    override def hashCode(): Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  object Element {
    implicit val tensorAlgebraElementSemigroup: Semigroup[Element] =
      Semigroup.instance((a, b) => a + b)
  }

  implicit class AMinusGeneratorExtensions(g: AMinus.Generator) {
    def toTensorAlgebra(tensorAlgebra: TensorAlgebra): TensorAlgebra.Generator =
      new TensorAlgebra.Generator(tensorAlgebra, IndexedSeq(g))
  }

  implicit class AMinusElementExtensions(g: AMinus.Element) {
    def toTensorAlgebra(tensorAlgebra: TensorAlgebra): TensorAlgebra.Element =
      new TensorAlgebra.Element(tensorAlgebra, g.terms.mapKeys(_.toTensorAlgebra(tensorAlgebra)))
  }
}
