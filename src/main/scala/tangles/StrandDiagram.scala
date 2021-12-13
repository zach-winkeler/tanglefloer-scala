package tangles

import scalaz.Scalaz._
import scalaz.Semigroup

import algebras.Sign.{Negative, Positive, Sign}
import algebras.{AMinus, Z2PolynomialRing}
import tangles.StrandUtils._
import utilities.Functions._

object StrandUtils {
  type Strand = (Float, Float)

  def cross(s1: Strand, s2: Strand): (Strand, Strand) = (s1._1 -> s2._2, s2._1 -> s1._2)

  def uncross(s1: Strand, s2: Strand): (Strand, Strand) = cross(s1, s2)

  implicit class StrandImprovements(base: Strand) {
    def crosses(other: Strand): Boolean = (base._1 < other._1) ^ (base._2 < other._2)

    def startsBelow(other: Strand): Boolean = base._1 < other._1

    def round: (Int, Int) = (base._1.round, base._2.round)

    def isStraight: Boolean = base._1 == base._2
  }
}

class StrandDiagram(val module: StrandDiagramSpan,
                    val blackStrands: Map[Float, Float],
                    val orangeStrands: IndexedSeq[Strand],
                    val orangeSigns: IndexedSeq[Sign],
                    val orangeVars: IndexedSeq[Z2PolynomialRing.Element]) {
  def toAMinusGenerator(algebra: AMinus): AMinus.Generator = new AMinus.Generator(algebra, blackStrands)

  def dPlus: StrandDiagramSpanElement = {
    var result = module.zero
    for (s1 <- blackStrands;
         s2 <- blackStrands if (s1 startsBelow s2) && (s1 crosses s2)) {
      var coefficient = module.ring.one
      for ((o, i) <- orangeStrands.view.zipWithIndex if (o crosses s1) && (o crosses s2)) {
        if (orangeSigns(i) == Positive) {
          coefficient *= orangeVars(i)
        } else {
          coefficient *= module.ring.zero
        }
      }
      result += coefficient *: this.uncrossed(s1, s2).toElement
    }
    result
  }

  def dMinus: StrandDiagramSpanElement = {
    var result = module.zero
    for (s1 <- blackStrands;
         s2 <- blackStrands if (s1 startsBelow s2) && !(s1 crosses s2)) {
      val (newS1, newS2) = cross(s1, s2)
      var coefficient = module.ring.one
      for ((o, i) <- orangeStrands.view.zipWithIndex if (o crosses newS1) && (o crosses newS2)) {
        if (orangeSigns(i) == Negative) {
          coefficient *= orangeVars(i)
        } else {
          coefficient *= module.ring.zero
        }
      }
      result += coefficient *: this.crossed(s1, s2).toElement
    }
    result
  }

  def *(other: StrandDiagram): StrandDiagramSpanElement = {
    var coefficient = module.ring.one
    var newHalfStrands = Set.empty[(Strand, Strand)]
    var newStrands = Map.empty[Float, Float]
    for (s1 <- this.blackStrands) {
      if (other.blackStrands contains s1._2) {
        val s2 = (s1._2, other.blackStrands(s1._2))
        for ((s3, s4) <- newHalfStrands) {
          if ((s1 crosses s3) && (s2 crosses s4)) {
            coefficient *= module.ring.zero
          }
        }
        newHalfStrands = newHalfStrands.incl((s1, s2))
        newStrands += s1._1 -> s2._2
        for ((o, i) <- orangeStrands.view.zipWithIndex if (o crosses s1) && (o crosses s2)) {
          if (orangeSigns(i) == Positive) {
            coefficient *= orangeVars(i)
          } else {
            coefficient *= module.ring.zero
          }
        }
      } else {
        coefficient *= module.ring.zero
      }
    }
    coefficient *: new StrandDiagram(module, newStrands, orangeStrands, orangeSigns, orangeVars).toElement
  }

  def crossed(s1: Strand, s2: Strand): StrandDiagram = {
    new StrandDiagram(this.module, blackStrands ++ cross(s1, s2), orangeStrands, orangeSigns, orangeVars)
  }

  def uncrossed(s1: Strand, s2: Strand): StrandDiagram = crossed(s1, s2)

  def toElement: StrandDiagramSpanElement = new StrandDiagramSpanElement(module, Map(this -> module.ring.one))

  def canEqual(other: Any): Boolean = other.isInstanceOf[StrandDiagram]

  override def equals(other: Any): Boolean = other match {
    case that: StrandDiagram =>
      (that canEqual this) &&
        module == that.module &&
        blackStrands == that.blackStrands &&
        orangeStrands == that.orangeStrands &&
        orangeSigns == that.orangeSigns &&
        orangeVars == that.orangeVars
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(blackStrands, orangeStrands, orangeSigns, orangeVars)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = blackStrands.map(_.round).toString.substring(3)
}

class StrandDiagramSpan(val ring: Z2PolynomialRing) {
  val zero: StrandDiagramSpanElement =
    new StrandDiagramSpanElement(this, Map.empty[StrandDiagram, Z2PolynomialRing.Element])
}

class StrandDiagramSpanElement(val module: StrandDiagramSpan, _terms: Map[StrandDiagram, Z2PolynomialRing.Element]) {
  val terms: Map[StrandDiagram, Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)
  
  def toAMinusElement(algebra: AMinus): AMinus.Element = {
    var result = algebra.zero
    for ((g, c) <- terms) {
      result += c *: g.toAMinusGenerator(algebra).toElement
    }
    result
  }

  def dPlus: StrandDiagramSpanElement = {
    var result = module.zero
    for ((g,c) <- terms) {
      result += c *: g.dPlus
    }
    result
  }

  def dMinus: StrandDiagramSpanElement = {
    var result = module.zero
    for ((g,c) <- terms) {
      result += c *: g.dMinus
    }
    result
  }

  def +(other: StrandDiagramSpanElement): StrandDiagramSpanElement = {
    assert (this.module == other.module)
    new StrandDiagramSpanElement(this.module, this.terms |+| other.terms)
  }

  def *:(scalar: Z2PolynomialRing.Element): StrandDiagramSpanElement =
    new StrandDiagramSpanElement(module, terms.view.mapValues(scalar * _).toMap)

  override def equals(other: Any): Boolean = other match {
    case other: StrandDiagramSpanElement => this.module == other.module && this.terms == other.terms
    case _ => false
  }

  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
}

object StrandDiagramSpanElement {
  implicit val strandDiagramSpanElementSemigroup: Semigroup[StrandDiagramSpanElement] =
    Semigroup.instance((a, b) => a + b)
}
