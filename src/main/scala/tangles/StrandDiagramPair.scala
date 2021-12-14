package tangles

import algebras.Sign._
import scalaz.Scalaz._
import scalaz.Semigroup
import tangles.StrandUtils._
import algebras.Z2PolynomialRing

class StrandDiagramPair(val module: StrandDiagramPairSpan, val left: StrandDiagram, val right: StrandDiagram) {
  def toElement: StrandDiagramPairSpanElement = new StrandDiagramPairSpanElement(module, Map(this -> module.ring.one))

  def deltaA: StrandDiagramPairSpanElement = deltaACase1 + deltaACase2 + deltaACase3 + deltaACase4

  def deltaACase1: StrandDiagramPairSpanElement = {
    var result = module.zero
    for (s1 <- left.blackStrands; s2 <- left.blackStrands if (s1 startsBelow s2) && !(s1 crosses s2)) {
      var coefficient = module.ring.one
      if (left.blackStrands.exists(deltaACase1LeftCondition(s1, s2, _))
        || right.blackStrands.exists(deltaACase1RightCondition(s1, s2, _))) {
        coefficient *= module.ring.zero
      }
      for (o <- left.orangeStrands if deltaACase1LeftCondition(s1, s2, o)) {
        if (o.sign == Positive) {
          coefficient *= o.variable
        } else {
          coefficient *= module.ring.zero
        }
      }
      for (o <- right.orangeStrands if deltaACase1RightCondition(s1, s2, o)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= module.ring.zero
        }
      }
      result += coefficient *: new StrandDiagramPair(module, left.crossed(s1, s2), right).toElement
    }
    result
  }

  def deltaACase1LeftCondition(s1: StrandLike, s2: StrandLike, s3: StrandLike): Boolean =
    (s3 endsBetween (s1,s2)) && ((s3 crosses s1) || (s3 crosses s2))

  def deltaACase1RightCondition(s1: StrandLike, s2: StrandLike, s3: StrandLike): Boolean =
    s3 startsBetweenEnds (s1,s2)

  def deltaACase2: StrandDiagramPairSpanElement = {
    var result = module.zero
    result
  }

  def deltaACase3: StrandDiagramPairSpanElement = {
    var result = module.zero
    result
  }

  def deltaACase4: StrandDiagramPairSpanElement = {
    var result = module.zero
    result
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[StrandDiagramPair]

  override def equals(other: Any): Boolean = other match {
    case that: StrandDiagramPair =>
      (that canEqual this) &&
        this.module == that.module &&
        this.left == that.left &&
        this.right == that.right
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(left, right)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = left.toString + right.toString
}

class StrandDiagramPairSpan(val ring: Z2PolynomialRing) {
  val zero: StrandDiagramPairSpanElement =
    new StrandDiagramPairSpanElement(this, Map.empty[StrandDiagramPair, Z2PolynomialRing.Element])
}

class StrandDiagramPairSpanElement(val module: StrandDiagramPairSpan,
                                   _terms: Map[StrandDiagramPair, Z2PolynomialRing.Element]) {
  val terms: Map[StrandDiagramPair, Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)

  def deltaA: StrandDiagramPairSpanElement = {
    var result = module.zero
    for ((g, c) <- terms) {
      result += c *: g.deltaA
    }
    result
  }

  def +(other: StrandDiagramPairSpanElement): StrandDiagramPairSpanElement = {
    assert (this.module == other.module)
    new StrandDiagramPairSpanElement(this.module, this.terms |+| other.terms)
  }

  def *:(scalar: Z2PolynomialRing.Element): StrandDiagramPairSpanElement =
    new StrandDiagramPairSpanElement(module, terms.view.mapValues(scalar * _).toMap)

  override def equals(other: Any): Boolean = other match {
    case other: StrandDiagramPairSpanElement => this.module == other.module && this.terms == other.terms
    case _ => false
  }

  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
}

object StrandDiagramPairSpanElement {
  implicit val strandDiagramPairSpanElementSemigroup: Semigroup[StrandDiagramPairSpanElement] =
    Semigroup.instance((a, b) => a + b)
}
