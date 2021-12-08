package algebra

import algebra.Sign.Sign
import scalaz.Scalaz._
import tangles.{StrandDiagram, StrandDiagramSpan}
import tangles.StrandUtils.Strand
import utilities.IndexedSeqUtils.IndexedSeqExtensions

object Sign extends Enumeration {
  protected case class SignVal(sign: Int) extends super.Val {
    def unary_- : SignVal = this match { case Positive => Negative case Negative => Positive }
  }
  type Sign = SignVal
  val Positive: SignVal = SignVal(1)
  val Negative: SignVal = SignVal(-1)

  import scala.language.implicitConversions
  implicit def valueToSignVal(x: Value): SignVal = x.asInstanceOf[SignVal]
}

class AMinus(val signs: IndexedSeq[Sign]) {
  val positives: IndexedSeq[Int] = signs.indicesWhere(_ == Sign.Positive)
  val ring = new Z2PolynomialRing(positives.map(i => s"u$i"))

  val zero: AMinusElement = new AMinusElement(this, Map.empty[AMinusGenerator, Z2Polynomial])

  val strandDiagramSpan: StrandDiagramSpan = new StrandDiagramSpan(ring)
  val orangeStrands: IndexedSeq[Strand] = signs.indices.map(y => (y + 0.5f, y + 0.5f))

  def gen(strands: Map[Float, Float]): AMinusGenerator = new AMinusGenerator(this, strands)
  def elt(strands: Map[Float, Float]): AMinusElement = gen(strands).toElement
}

class AMinusGenerator(val algebra: AMinus, val strands: Map[Float, Float]) {
  def toElement: AMinusElement = new AMinusElement(algebra, Map(this -> algebra.ring.one))

  def toStrandDiagram: StrandDiagram = {
    new StrandDiagram(algebra.strandDiagramSpan, strands, algebra.orangeStrands, algebra.signs, algebra.ring.vars)
  }

  def d: AMinusElement = this.toStrandDiagram.dPlus.toAMinusElement(algebra)

  def *(other: AMinusGenerator): AMinusElement =
    (this.toStrandDiagram * other.toStrandDiagram).toAMinusElement(this.algebra)

  override def equals(other: Any): Boolean = other match {
    case other: AMinusGenerator => this.algebra == other.algebra && this.strands == other.strands
    case _ => false
  }

  override def hashCode: Int = Seq(strands).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = strands.toString
}

class AMinusElement(val algebra: AMinus, _terms: Map[AMinusGenerator, Z2Polynomial]) {
  val terms: Map[AMinusGenerator, Z2Polynomial] = _terms.filter(_._2 != algebra.ring.zero)

  def +(other: AMinusElement): AMinusElement = {
    assert (this.algebra == other.algebra)
    new AMinusElement(this.algebra, this.terms |+| other.terms)
  }

  def d: AMinusElement = {
    var result = this.algebra.zero
    for ((g,c) <- this.terms) {
      result += c *: g.d
    }
    result
  }

  def *(other: AMinusElement): AMinusElement = {
    assert (this.algebra == other.algebra)
    var result = this.algebra.zero
    for ((g1, c1) <- this.terms; (g2, c2) <- other.terms) {
      result += (c1 * c2) *: (g1 * g2)
    }
    result
  }

  def *:(scalar: Z2Polynomial): AMinusElement =
    new AMinusElement(algebra, terms.view.mapValues(scalar * _).toMap)

  def canEqual(other: Any): Boolean = other.isInstanceOf[AMinusElement]

  override def equals(other: Any): Boolean = other match {
    case that: AMinusElement =>
      (that canEqual this) &&
        terms == that.terms &&
        algebra == that.algebra
    case _ => false
  }

  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
}