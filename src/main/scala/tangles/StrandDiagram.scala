package tangles

import algebra.Sign.{Positive, Sign}
import algebra.{AMinus, AMinusElement, AMinusGenerator, Z2Polynomial, Z2PolynomialRing}
import scalaz.Scalaz._
import scalaz.Semigroup
import tangles.StrandUtils.{Strand, StrandImprovements}

object StrandUtils {
  type Strand = (Float, Float)

  implicit class StrandImprovements(base: Strand) {
    def crosses(other: Strand): Boolean = (base._1 < other._1) ^ (base._2 < other._2)
  }
}

class StrandDiagram(val module: StrandDiagramSpan,
                    val blackStrands: Map[Float, Float],
                    val orangeStrands: IndexedSeq[Strand],
                    val orangeSigns: IndexedSeq[Sign],
                    val orangeVars: IndexedSeq[Z2Polynomial]) {
  def toAMinusGenerator(algebra: AMinus): AMinusGenerator = new AMinusGenerator(algebra, blackStrands)

  def dPlus: StrandDiagramSpanElement = {
    var result = module.zero
    for (s1 <- blackStrands;
         s2 <- blackStrands if (s1._1 < s2._1) && (s1 crosses s2)) {
      var coefficient = module.ring.one
      for ((o, i) <- orangeStrands.view.zipWithIndex if (o crosses s1) && (o crosses s2)) {
        if (orangeSigns(i) == Positive) {
          coefficient *= orangeVars(i)
        } else {
          coefficient *= module.ring.zero
        }
      }
      result += coefficient *: this.uncross(s1, s2).toElement
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

  def uncross(s1: Strand, s2: Strand): StrandDiagram = {
    val newBlackStrands = blackStrands ++ List(s1._1 -> s2._2, s2._1 -> s1._2)
    new StrandDiagram(this.module, newBlackStrands, orangeStrands, orangeSigns, orangeVars)
  }

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
}

class StrandDiagramSpan(val ring: Z2PolynomialRing) {
  val zero: StrandDiagramSpanElement =
    new StrandDiagramSpanElement(this, Map.empty[StrandDiagram, Z2Polynomial])
}

class StrandDiagramSpanElement(val module: StrandDiagramSpan, val terms: Map[StrandDiagram, Z2Polynomial]) {
  def toAMinusElement(algebra: AMinus): AMinusElement = {
    var result = algebra.zero
    for ((g, c) <- terms) {
      result += c *: g.toAMinusGenerator(algebra).toElement
    }
    result
  }

  def +(other: StrandDiagramSpanElement): StrandDiagramSpanElement = {
    assert (this.module == other.module)
    new StrandDiagramSpanElement(this.module, this.terms |+| other.terms)
  }

  def *:(scalar: Z2Polynomial): StrandDiagramSpanElement =
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
