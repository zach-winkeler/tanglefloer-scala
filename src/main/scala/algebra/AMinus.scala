package algebra

import algebra.Sign.Sign
import scalaz.Scalaz._
import utilities.VectorUtils.VectorImprovements

object Sign extends Enumeration {
  type Sign = Value
  val Positive: Value = Value(1)
  val Negative: Value = Value(-1)
}

class AMinus(val signSequence: Vector[Sign]) {
  val positives: Vector[Int] = this.signSequence.indicesWhere(_ == Sign.Positive)
  val ring = new Z2PolynomialRing(this.positives.map(i => s"u$i").toSet)

  val zero: AMinusElement = new AMinusElement(this, Map.empty[AMinusGenerator, Z2Polynomial])

  def gen(strands: Map[Int, Int]): AMinusGenerator = new AMinusGenerator(this, strands)
  def elt(strands: Map[Int, Int]): AMinusElement = this.gen(strands).toElement
}

class AMinusGenerator(val algebra: AMinus, val strands: Map[Int, Int]) {
  def toElement: AMinusElement = new AMinusElement(this.algebra, Map(this -> this.algebra.ring.one))

  def *(other: AMinusGenerator): AMinusElement = {
    this.algebra.zero
  }

  override def equals(other: Any): Boolean = other match {
    case other: AMinusGenerator => this.algebra == other.algebra && this.strands == other.strands
    case _ => false
  }

  override def hashCode: Int = Seq(strands).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = this.strands.toString
}

class AMinusElement(val algebra: AMinus, val terms: Map[AMinusGenerator, Z2Polynomial]) {
  def +(other: AMinusElement): AMinusElement = {
    assert(this.algebra == other.algebra)
    new AMinusElement(this.algebra, this.terms |+| other.terms)
  }

  def *(other: AMinusElement): AMinusElement = {
    assert(this.algebra == other.algebra)
    var result = this.algebra.zero
    for ((g1, c1) <- this.terms) {
      for ((g2, c2) <- other.terms) {
        result += (c1 * c2) *: (g1 * g2)
      }
    }
    result
  }

  def *:(scalar: Z2Polynomial): AMinusElement =
    new AMinusElement(this.algebra, this.terms.mapValues(scalar * _).toMap)

  override def equals(other: Any): Boolean = other match {
    case other: AMinusElement => this.algebra == other.algebra && this.terms == other.terms
    case _ => false
  }

  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = this.terms.mkString(" + ") match { case "" => "0" case s => s }
}