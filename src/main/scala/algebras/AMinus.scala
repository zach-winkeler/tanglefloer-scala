package algebras

import scala.language.implicitConversions
import algebras.Sign.{Negative, Positive, Sign}
import tangles.{Strand, VariableStrand}
import tangles.StrandUtils._
import utilities.Functions._
import utilities.IndexedSeqUtils.IndexedSeqExtensions
import cats.implicits._

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

class AMinus(val signs: IndexedSeq[Set[Sign]]) {
  import AMinus._
  val positives: IndexedSeq[Int] = signs.indicesWhere(_.contains(Positive))
  val ring = new Z2PolynomialRing(positives.indices.map(i => s"u$i"))

  val zero: Element = new Element(this, Map.empty[AMinus.Generator, Z2PolynomialRing.Element])

  val orangeStrands: Set[VariableStrand] =
    signs.indices.flatMap(y => signs(y).map {
      case Positive => VariableStrand(y + 0.5f, y + 0.5f, Positive, ring.vars(positives.indexOf(y)))
      case Negative => VariableStrand(y + 0.5f, y + 0.5f, Negative, ring.zero)
      case _ => throw new RuntimeException("invalid sign")
    }).toSet

  def idempotent(occupied: Iterable[Float]): Generator = gen(occupied.map(p => Strand(p, p)).toSet)
  def gen(strands: Set[Strand]): Generator = new AMinus.Generator(this, strands)
  def elt(strands: Set[Strand]): Element = gen(strands).toElement

  def gens: Set[Generator] =
    partialBijections(
      (0 until signs.length+1).map(_.toFloat).toSet,
      (0 until signs.length+1).map(_.toFloat).toSet
    ).map(s => gen(s.map(t => Strand(t._1, t._2))))

  def canEqual(other: Any): Boolean = other.isInstanceOf[AMinus]

  override def equals(other: Any): Boolean = other match {
    case that: AMinus =>
      (that canEqual this) &&
        signs == that.signs
    case _ => false
  }

  override def hashCode(): Int = Seq(signs).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}

object AMinus {
  class Generator(val algebra: AMinus, val strands: Set[Strand]) {
    def toElement: Element = new Element(algebra, Map(this -> algebra.ring.one))

    def isIdempotent: Boolean = strands.forall(_.isStraight)

    def leftIdempotent: AMinus.Generator = algebra.idempotent(strands.map(_.start))

    def rightIdempotent: AMinus.Generator = algebra.idempotent(strands.map(_.end))

    def complement: AMinus.Generator = {
      assert(strands.forall(s => s.start == s.end))
      algebra.gen((0 until algebra.signs.length+1).toSet[Int].map(y => Strand(y.toFloat, y.toFloat)) &~ strands)
    }

    def uncrossed(s1: Strand, s2: Strand): Generator =
      algebra.gen(strands -- List(s1, s2) ++ tuple2ToIndexedSeq(uncross(s1, s2)))

    def d: Element = {
      var result = algebra.zero
      for (s1 <- strands; s2 <- strands if (s1 startsBelow s2) && (s1 crosses s2)) {
        var coefficient = algebra.ring.one
        for (s <- strands.map(_.toVariableStrand(algebra.ring.zero)) | algebra.orangeStrands
             if (s startsBetween (s1,s2)) && (s endsBetween (s2,s1))) {
          coefficient *= s.variable
        }
        result += coefficient *: this.uncrossed(s1, s2).toElement
      }
      result
    }

    def *(other: Generator): Element = {
      var coefficient = algebra.ring.one
      var newHalfStrands = Set.empty[(Strand, Strand)]
      var newStrands = Set.empty[Strand]
      for (s1 <- this.strands) {
        other.strands.find(_.start == s1.end) match {
          case Some(s2) =>
            for ((s3, s4) <- newHalfStrands) {
              if ((s1 crosses s3) && (s2 crosses s4)) {
                coefficient *= algebra.ring.zero
              }
            }
            newHalfStrands = newHalfStrands.incl((s1, s2))
            newStrands += s1.start -> s2.end
            for (o <- algebra.orangeStrands if (o crosses s1) && (o crosses s2)) {
              coefficient *= o.variable
            }
          case None =>
            coefficient *= algebra.ring.zero
        }
      }
      coefficient *: algebra.gen(newStrands).toElement
    }

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
