package algebra

import algebra.Sign.Sign
import scalaz.Scalaz._
import utilities.VectorUtils.VectorImprovements

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

class AMinus(val signs: Vector[Sign]) {
  val positives: Vector[Int] = signs.indicesWhere(_ == Sign.Positive)
  val ring = new Z2PolynomialRing(positives.map(i => s"u$i").toSet)

  val zero: AMinusElement = new AMinusElement(this, Map.empty[AMinusGenerator, Z2Polynomial])

  def gen(strands: Map[Int, Int]): AMinusGenerator = new AMinusGenerator(this, strands)
  def elt(strands: Map[Int, Int]): AMinusElement = gen(strands).toElement
}

class AMinusGenerator(val algebra: AMinus, val strands: Map[Int, Int]) {
  def toElement: AMinusElement = new AMinusElement(algebra, Map(this -> algebra.ring.one))

  def *(other: AMinusGenerator): AMinusElement = {
    algebra.zero
  }

  override def equals(other: Any): Boolean = other match {
    case other: AMinusGenerator => this.algebra == other.algebra && this.strands == other.strands
    case _ => false
  }

  override def hashCode: Int = Seq(strands).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = strands.toString
}

class AMinusElement(val algebra: AMinus, val terms: Map[AMinusGenerator, Z2Polynomial]) {
  def +(other: AMinusElement): AMinusElement = {
    assert (this.algebra == other.algebra)
    new AMinusElement(this.algebra, this.terms |+| other.terms)
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
    new AMinusElement(algebra, terms.mapValues(scalar * _).toMap)

  override def equals(other: Any): Boolean = other match {
    case other: AMinusElement => this.algebra == other.algebra && this.terms == other.terms
    case _ => false
  }

  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
}