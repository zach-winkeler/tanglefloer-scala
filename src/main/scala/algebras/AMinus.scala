package algebras

import algebras.Sign.Sign
import scalaz.Scalaz._
import tangles.{StrandDiagram, StrandDiagramSpan}
import tangles.StrandUtils._
import utilities.Functions.partialBijections
import utilities.IndexedSeqUtils.IndexedSeqExtensions

object Sign extends Enumeration {
  protected case class SignVal(sign: Int) extends super.Val {
    def unary_- : SignVal = this match {
      case Positive => Negative
      case Negative => Positive
      case _ => throw new RuntimeException("invalid sign type")
    }
  }
  type Sign = SignVal
  val Positive: SignVal = SignVal(1)
  val Negative: SignVal = SignVal(-1)

  import scala.language.implicitConversions
  implicit def valueToSignVal(x: Value): SignVal = x.asInstanceOf[SignVal]
}

class AMinus(val signs: IndexedSeq[Sign]) {
  import AMinus._
  val positives: IndexedSeq[Int] = signs.indicesWhere(_ == Sign.Positive)
  val ring = new Z2PolynomialRing(positives.map(i => s"u$i"))

  val zero: Element = new Element(this, Map.empty[AMinus.Generator, Z2PolynomialRing.Element])

  val strandDiagramSpan: StrandDiagramSpan = new StrandDiagramSpan(ring)
  val orangeStrands: IndexedSeq[Strand] = signs.indices.map(y => (y + 0.5f, y + 0.5f))

  def idempotent(occupied: Iterable[Float]): Generator = gen(occupied.map(p => p -> p).toMap)
  def gen(strands: Map[Float, Float]): Generator = new AMinus.Generator(this, strands)
  def elt(strands: Map[Float, Float]): Element = gen(strands).toElement

  def gens: Set[Generator] =
    partialBijections(
      (0 until signs.length+1).map(_.toFloat).toSet,
      (0 until signs.length+1).map(_.toFloat).toSet
    ).map(gen)
}

object AMinus {
  class Generator(val algebra: AMinus, val strands: Map[Float, Float]) {
    def toElement: Element = new Element(algebra, Map(this -> algebra.ring.one))

    def isIdempotent: Boolean = strands.forall(_.isStraight)

    def leftIdempotent: AMinus.Generator = algebra.idempotent(strands.keys)

    def rightIdempotent: AMinus.Generator = algebra.idempotent(strands.values)

    def toStrandDiagram: StrandDiagram = {
      new StrandDiagram(algebra.strandDiagramSpan, strands, algebra.orangeStrands, algebra.signs, algebra.ring.vars)
    }

    def d: Element = this.toStrandDiagram.dPlus.toAMinusElement(algebra)

    def *(other: Generator): Element =
      (this.toStrandDiagram * other.toStrandDiagram).toAMinusElement(this.algebra)

    override def equals(other: Any): Boolean = other match {
      case other: Generator => this.algebra == other.algebra && this.strands == other.strands
      case _ => false
    }

    override def hashCode: Int = Seq(strands).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = strands.map(_.round).toString.substring(3)
  }

  class Element(val algebra: AMinus, _terms: Map[AMinus.Generator, Z2PolynomialRing.Element]) {
    val terms: Map[AMinus.Generator, Z2PolynomialRing.Element] = _terms.filter(_._2 != algebra.ring.zero)

    def +(other: Element): Element = {
      assert(this.algebra == other.algebra)
      new Element(this.algebra, this.terms |+| other.terms)
    }

    def d: Element = {
      var result = this.algebra.zero
      for ((g, c) <- this.terms) {
        result += c *: g.d
      }
      result
    }

    def *(other: Element): Element = {
      assert(this.algebra == other.algebra)
      var result = this.algebra.zero
      for ((g1, c1) <- this.terms; (g2, c2) <- other.terms) {
        result += (c1 * c2) *: (g1 * g2)
      }
      result
    }

    def *:(scalar: Z2PolynomialRing.Element): Element =
      new Element(algebra, terms.view.mapValues(scalar * _).toMap)

    def canEqual(other: Any): Boolean = other.isInstanceOf[Element]

    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        (that canEqual this) &&
          terms == that.terms &&
          algebra == that.algebra
      case _ => false
    }

    override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = terms.mkString(" + ") match {
      case "" => "0"
      case s => s
    }
  }
}
