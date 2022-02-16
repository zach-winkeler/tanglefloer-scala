package algebras

import utilities.MapUtils.MapExtensions
import cats.Semigroup
import cats.implicits._

class Z2PolynomialRing(val varNames: IndexedSeq[Object]) {
  val zero: Z2PolynomialRing.Element = new Z2PolynomialRing.Element(this, Set())
  val one: Z2PolynomialRing.Element = new Z2PolynomialRing.Monomial(this, Map()).toPolynomial
  val vars: IndexedSeq[Z2PolynomialRing.Element] = varNames.map(this(_))

  override def equals(other: Any): Boolean = other match {
    case other: Z2PolynomialRing => this.varNames == other.varNames
    case _ => false
  }

  def apply(varName: Object): Z2PolynomialRing.Element = {
    if (varNames.contains(varName)) {
      new Z2PolynomialRing.Monomial(this, Map(varName -> 1)).toPolynomial
    } else {
      throw new RuntimeException("no such variable exists")
    }
  }

  def tensorInclusions(other: Z2PolynomialRing): (Z2PolynomialRing.Morphism, Z2PolynomialRing.Morphism) = {
    val z2 = new Z2PolynomialRing(IndexedSeq())
    val f = new Z2PolynomialRing.Morphism(z2, this, Map.empty)
    val g = new Z2PolynomialRing.Morphism(z2, other, Map.empty)
    f.pushoutInclusions(g)
  }

  override def hashCode(): Int = {
    val state = Seq(varNames)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Z2PolynomialRing {
  class Morphism(val source: Z2PolynomialRing,
                 val target: Z2PolynomialRing,
                 val mapping: Map[Object, Object]) {
    assert(mapping.keySet == source.varNames.toSet)
    assert(mapping.values.toSet.subsetOf(target.varNames.toSet))

    def inverse = new Morphism(target, source, mapping.map(_.swap))

    def apply(x: Monomial): Monomial =
      new Monomial(target, x.powers.mapKeysAndMerge(k => mapping(k), _+_))

    def apply(x: Element): Element = {
      if (x.ring != source) {
        throw new RuntimeException("cannot apply map to an element of a different ring")
      } else {
        x.terms.foldLeft(target.zero)((acc, term) => acc + apply(term).toPolynomial)
      }
    }

    def compose(other: Z2PolynomialRing.Morphism): Z2PolynomialRing.Morphism = {
      assert(this.source == other.target)
      new Z2PolynomialRing.Morphism(other.source, this.target,
        other.mapping.view.mapValues(this.mapping(_)).toMap)
    }

    def retract(y: Element) : Element = inverse.apply(y)

    def pushoutInclusions(other: Z2PolynomialRing.Morphism): (Z2PolynomialRing.Morphism, Z2PolynomialRing.Morphism) = {
      assert(this.source == other.source)
      val leftVars = this.target.varNames.toSet &~ this.mapping.values.toSet
      val commonVars = this.source.varNames.toSet
      val rightVars = other.target.varNames.toSet &~ other.mapping.values.toSet
      val pushout = new Z2PolynomialRing(
        (leftVars.map((_,1)) ++ commonVars.map((_,2)) ++ rightVars.map((_,3)))
          .toIndexedSeq)
      val inLeft = new Z2PolynomialRing.Morphism(this.target, pushout,
        (leftVars.map(v => v -> (v,1)) ++ commonVars.map(v => this.mapping(v) -> (v,2))).toMap)
      val inRight = new Z2PolynomialRing.Morphism(other.target, pushout,
        (rightVars.map(v => v -> (v,3)) ++ commonVars.map(v => other.mapping(v) -> (v,2))).toMap)
      (inLeft, inRight)
    }
  }

  object Morphism {
    def identity(ring: Z2PolynomialRing): Morphism =
      new Morphism(ring, ring, ring.varNames.foldLeft(Map.empty[Object, Object])((acc, x) => acc + (x -> x)))
  }

  class Monomial(val ring: Z2PolynomialRing, val powers: Map[Object, Int]) {
    def toPolynomial: Element = new Element(ring, Set(this))

    def degree: Int = powers.foldLeft(0)(_+_._2)

    def *(other: Monomial): Monomial = {
      assert(this.ring == other.ring)
      new Monomial(this.ring, this.powers |+| other.powers)
    }

    override def equals(other: Any): Boolean = other match {
      case other: Monomial => this.ring == other.ring && this.powers == other.powers
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
      if (result == "") {
        "1"
      } else {
        result.dropRight(1)
      }
    }
  }

  class Element(val ring: Z2PolynomialRing, val terms: Set[Monomial]) {
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

    def +(other: Element): Element = {
      assert (this.ring == other.ring)
      new Element(this.ring, (this.terms | other.terms) &~ (this.terms & other.terms))
    }

    def *(other: Element): Element = {
      assert (this.ring == other.ring)
      var result = this.ring.zero
      for (t1 <- this.terms; t2 <- other.terms) {
        result += (t1 * t2).toPolynomial
      }
      result
    }

    override def equals(other: Any): Boolean = other match {
      case other: Element => this.ring == other.ring && this.terms == other.terms
      case _ => false
    }

    override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
  }

  object Element {
    implicit val z2PolynomialSemigroup: Semigroup[Element] =
      Semigroup.instance((a, b) => a + b)
  }
}





