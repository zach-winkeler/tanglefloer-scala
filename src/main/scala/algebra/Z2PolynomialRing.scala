package algebra

import scalaz.Scalaz._
import scalaz.Semigroup
import utilities.MapUtils.MapImprovements

class Z2PolynomialRing(val variables: IndexedSeq[Object]) {
  val zero: Z2Polynomial = new Z2Polynomial(this, Set())
  val one: Z2Polynomial = new Z2Monomial(this, Map()).toPolynomial
  val vars: IndexedSeq[Z2Polynomial] = variables.map(this(_))

  override def equals(other: Any): Boolean = other match {
    case other: Z2PolynomialRing => this.variables == other.variables
    case _ => false
  }

  def apply(variable: Object): Z2Polynomial = {
    if (variables.contains(variable)) {
      new Z2Monomial(this, Map(variable -> 1)).toPolynomial
    } else {
      throw new RuntimeException("no such variable exists")
    }
  }
}

class Z2PolynomialRingMap(val source: Z2PolynomialRing,
                          val target: Z2PolynomialRing,
                          val mapping: Map[Object, Object]) {

  def inverse = new Z2PolynomialRingMap(target, source, mapping.map(_.swap))

  def apply(x: Z2Monomial): Z2Monomial =
    new Z2Monomial(target, x.powers.mapKeysAndMerge(k => mapping(k), _+_))

  def apply(x: Z2Polynomial): Z2Polynomial = {
    if (x.ring != source) {
      throw new RuntimeException("cannot apply map to an element of a different ring")
    } else {
      x.terms.foldLeft(target.zero)((acc, term) => acc + apply(term).toPolynomial)
    }
  }

  def retract(y: Z2Polynomial) : Z2Polynomial = inverse.apply(y)
}

object Z2PolynomialRingMap {
  def identity(ring: Z2PolynomialRing): Z2PolynomialRingMap =
    new Z2PolynomialRingMap(ring, ring, ring.variables.foldLeft(Map.empty[Object, Object])((acc, x) => acc + (x -> x)))
}

class Z2Monomial(val ring: Z2PolynomialRing, val powers: Map[Object, Int]) {
  def toPolynomial: Z2Polynomial = new Z2Polynomial(ring, Set(this))

  def degree: Int = powers.foldLeft(0)(_+_._2)

  def *(other: Z2Monomial): Z2Monomial = {
    assert(this.ring == other.ring)
    new Z2Monomial(this.ring, this.powers |+| other.powers)
  }

  override def equals(other: Any): Boolean = other match {
    case other: Z2Monomial => this.ring == other.ring && this.powers == other.powers
    case _ => false
  }

  override def hashCode: Int = Seq(powers).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = {
    var result = ""
    for ((variable, power) <- powers) {
      if (power == 1) {
        result += variable.toString + "."
      } else if (power > 1) {
        result += variable.toString + "^" + power.toString + "."
      }
    }
    result.dropRight(1)
  }
}

class Z2Polynomial(val ring: Z2PolynomialRing, val terms: Set[Z2Monomial]) {
  def degree: Int = {
    if (terms.isEmpty) {
      throw new RuntimeException("the zero polynomial has degree negative infinity")
    }
    val d = terms.head.degree
    if (!terms.tail.forall(_.degree == d)) {
      throw new RuntimeException("non-homogeneous polynomial")
    }
    d
  }

  def +(other: Z2Polynomial): Z2Polynomial = {
    assert (this.ring == other.ring)
    new Z2Polynomial(this.ring, (this.terms | other.terms) &~ (this.terms & other.terms))
  }

  def *(other: Z2Polynomial): Z2Polynomial = {
    assert (this.ring == other.ring)
    var result = this.ring.zero
    for (t1 <- this.terms; t2 <- other.terms) {
      result += (t1 * t2).toPolynomial
    }
    result
  }

  override def equals(other: Any): Boolean = other match {
    case other: Z2Polynomial => this.ring == other.ring && this.terms == other.terms
    case _ => false
  }

  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
}

object Z2Polynomial {
  implicit val z2PolynomialSemigroup: Semigroup[Z2Polynomial] =
    Semigroup.instance((a, b) => a + b)
}