package algebras

import scala.language.implicitConversions

import algebras.Sign.{Positive, Sign}
import scalaz.Scalaz._
import tangles.{StrandDiagram, StrandDiagramSpan, StrandDiagramSpanElement}
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

  val orangeStrands: Set[VariableStrand] =
    signs.indices.map(y => VariableStrand(y+0.5f, y+0.5f, signs(y),
      if (signs(y) == Positive) ring.vars(positives.indexOf(y)) else ring.zero)).toSet
  val strandDiagramSpan: StrandDiagramSpan = new StrandDiagramSpan(ring, orangeStrands)

  def idempotent(occupied: Iterable[Float]): Generator = gen(occupied.map(p => Strand(p, p)).toSet)
  def gen(strands: Set[Strand]): Generator = new AMinus.Generator(this, strands)
  def elt(strands: Set[Strand]): Element = gen(strands).toElement

  def gens: Set[Generator] =
    partialBijections(
      (0 until signs.length+1).map(_.toFloat).toSet,
      (0 until signs.length+1).map(_.toFloat).toSet
    ).map(s => gen(s.map(t => Strand(t._1, t._2))))

  def orangeVars: IndexedSeq[Z2PolynomialRing.Element] =
    signs.view.zipWithIndex.map(si => if (si._1 == Positive) ring.vars(si._2) else ring.zero).toIndexedSeq
}

object AMinus {
  class Generator(val algebra: AMinus, val strands: Set[Strand]) {
    def toElement: Element = new Element(algebra, Map(this -> algebra.ring.one))

    def isIdempotent: Boolean = strands.forall(_.isStraight)

    def leftIdempotent: AMinus.Generator = algebra.idempotent(strands.map(_.start))

    def rightIdempotent: AMinus.Generator = algebra.idempotent(strands.map(_.end))

    def asStrandDiagram: StrandDiagram = {
      new StrandDiagram(algebra.strandDiagramSpan, strands)
    }

    def d: Element = this.asStrandDiagram.dPlus.toAMinusElement(algebra)

    def *(other: Generator): Element =
      (this.asStrandDiagram * other.asStrandDiagram).toAMinusElement(this.algebra)

    override def equals(other: Any): Boolean = other match {
      case other: Generator => this.algebra == other.algebra && this.strands == other.strands
      case _ => false
    }

    override def hashCode: Int = Seq(strands).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = strands.map(_.round).toString.substring(3)
  }

  class Element(val algebra: AMinus, _terms: Map[AMinus.Generator, Z2PolynomialRing.Element]) {
    val terms: Map[AMinus.Generator, Z2PolynomialRing.Element] = _terms.filter(_._2 != algebra.ring.zero)

    def asStrandDiagramSpanElement: StrandDiagramSpanElement = {
      var result = algebra.strandDiagramSpan.zero
      for ((g, c) <- this.terms) {
        result += c *: g.asStrandDiagram.toElement
      }
      result
    }

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
