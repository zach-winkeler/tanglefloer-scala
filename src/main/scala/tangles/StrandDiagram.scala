package tangles

import scala.language.implicitConversions

import algebras.Sign.Sign
import algebras.Z2PolynomialRing

trait StrandLike {
  def start: Float
  def end: Float
}

trait DirectedStrandLike extends StrandLike {
  def sign: Sign
}

trait VariableStrandLike extends DirectedStrandLike {
  def variable: Z2PolynomialRing.Element
}

case class Strand(start: Float, end: Float) extends StrandLike {
  override def toString: String = start.toString + " -> " + end.toString
}

object Strand {
  implicit def fromTuple(ys: (Float, Float)): Strand = Strand(ys._1, ys._2)
}

case class DirectedStrand(start: Float, end: Float, sign: Sign) extends DirectedStrandLike {
  implicit def toStrand: Strand = (start, end)
}

case class VariableStrand(start: Float, end: Float, sign: Sign, variable: Z2PolynomialRing.Element)
  extends VariableStrandLike {
  implicit def toStrand: Strand = (start, end)

  implicit def toDirectedStrand: DirectedStrand = DirectedStrand(start, end, sign)
}

object StrandUtils {
  def cross(s1: Strand, s2: Strand): (Strand, Strand) = (s1.start -> s2.end, s2.start -> s1.end)

  def uncross(s1: Strand, s2: Strand): (Strand, Strand) = cross(s1, s2)

  implicit class StrandExtensions(base: StrandLike) {
    def crosses(other: StrandLike): Boolean = (base.start < other.start) ^ (base.end < other.end)

    def startsAbove(other: StrandLike): Boolean = base.start > other.start

    def startsBelow(other: StrandLike): Boolean = base.start < other.start

    def startsBetween(others: (StrandLike, StrandLike)): Boolean = _startsBetween.tupled(others)

    private val _startsBetween: (StrandLike, StrandLike) => Boolean =
      (lower, upper) => (base startsAbove lower) && (base startsBelow upper)

    def endsAbove(other: StrandLike): Boolean = base.end > other.end

    def endsBelow(other: StrandLike): Boolean = base.end < other.end

    def endsBetween(others: (StrandLike, StrandLike)): Boolean = _endsBetween.tupled(others)

    private val _endsBetween: (StrandLike, StrandLike) => Boolean =
      (lower, upper) => (base endsAbove lower) && (base endsBelow upper)

    def startsAboveEnd(other: StrandLike): Boolean = base.start > other.end

    def startsBelowEnd(other: StrandLike): Boolean = base.start < other.end

    def startsBetweenEnds(others: (StrandLike, StrandLike)): Boolean = _startsBetweenEnds.tupled(others)

    private val _startsBetweenEnds: (StrandLike, StrandLike) => Boolean =
      (lower, upper) => (base startsAboveEnd lower) && (base startsBelowEnd upper)

    def endsAboveStart(other: StrandLike): Boolean = base.end > other.start

    def endsBelowStart(other: StrandLike): Boolean = base.end < other.start

    def endsBetweenStarts(others: (StrandLike, StrandLike)): Boolean = _endsBetweenStarts.tupled(others)

    private val _endsBetweenStarts: (StrandLike, StrandLike) => Boolean =
      (lower, upper) => (base endsAboveStart lower) && (base endsBelowStart upper)

    def round: IntStrand = IntStrand(base.start.round, base.end.round)

    def isStraight: Boolean = base.start == base.end

    case class IntStrand(start: Int, end: Int) {
      override def toString: String = start.toString + " -> " + end.toString
    }
  }
}

object PartialBijectionUtils {
  implicit class PartialBijectionExtensions(pb: Set[Strand]) {
    def sourceId: Set[Strand] = pb.map(s => s.start -> s.start)

    def targetId: Set[Strand] = pb.map(s => s.end -> s.end)
  }
}

//class StrandDiagram(val module: StrandDiagramSpan, val blackStrands: Set[Strand]) {
//  def toAMinusGenerator(algebra: AMinus): AMinus.Generator = new AMinus.Generator(algebra, blackStrands)
//
//  def leftIdempotentStrands: Set[Strand] =
//    blackStrands.map(s => s.start -> s.start)
//
//  def rightIdempotentStrands: Set[Strand] =
//    blackStrands.map(s => s.end -> s.end)
//
//  def dPlus: StrandDiagramSpanElement = {
//    var result = module.zero
//    for (s1 <- blackStrands;
//         s2 <- blackStrands if (s1 startsBelow s2) && (s1 crosses s2)) {
//      var coefficient = module.ring.one
//      for (o <- module.orangeStrands if (o crosses s1) && (o crosses s2)) {
//        if (o.sign == Positive) {
//          coefficient *= o.variable
//        } else {
//          coefficient *= module.ring.zero
//        }
//      }
//      result += coefficient *: this.uncrossed(s1, s2).toElement
//    }
//    result
//  }
//
//  def dMinus: StrandDiagramSpanElement = {
//    var result = module.zero
//    for (s1 <- blackStrands;
//         s2 <- blackStrands if (s1 startsBelow s2) && !(s1 crosses s2)) {
//      val (newS1, newS2) = cross(s1, s2)
//      var coefficient = module.ring.one
//      for (o <- module.orangeStrands if (o crosses newS1) && (o crosses newS2)) {
//        if (o.sign == Negative) {
//          coefficient *= o.variable
//        } else {
//          coefficient *= module.ring.zero
//        }
//      }
//      result += coefficient *: this.crossed(s1, s2).toElement
//    }
//    result
//  }
//
//  def *(other: StrandDiagram): StrandDiagramSpanElement = {
//    var coefficient = module.ring.one
//    var newHalfStrands = Set.empty[(Strand, Strand)]
//    var newStrands = Set.empty[Strand]
//    for (s1 <- this.blackStrands) {
//      other.blackStrands.find(_.start == s1.end) match {
//        case Some(s2) =>
//          for ((s3, s4) <- newHalfStrands) {
//            if ((s1 crosses s3) && (s2 crosses s4)) {
//              coefficient *= module.ring.zero
//            }
//          }
//          newHalfStrands = newHalfStrands.incl((s1, s2))
//          newStrands += s1.start -> s2.end
//          for (o <- module.orangeStrands if (o crosses s1) && (o crosses s2)) {
//            if (o.sign == Positive) {
//              coefficient *= o.variable
//            } else {
//              coefficient *= module.ring.zero
//            }
//          }
//        case None =>
//          coefficient *= module.ring.zero
//      }
//    }
//    coefficient *: new StrandDiagram(module, newStrands).toElement
//  }
//
//  def crossed(s1: Strand, s2: Strand): StrandDiagram = {
//    new StrandDiagram(this.module, blackStrands -- List(s1, s2) ++ cross(s1, s2))
//  }
//
//  def uncrossed(s1: Strand, s2: Strand): StrandDiagram = crossed(s1, s2)
//
//  def toElement: StrandDiagramSpanElement = new StrandDiagramSpanElement(module, Map(this -> module.ring.one))
//
//  def canEqual(other: Any): Boolean = other.isInstanceOf[StrandDiagram]
//
//  override def equals(other: Any): Boolean = other match {
//    case that: StrandDiagram =>
//      (that canEqual this) &&
//        module == that.module &&
//        blackStrands == that.blackStrands
//    case _ => false
//  }
//
//  override def hashCode(): Int = {
//    val state = Seq(blackStrands)
//    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
//  }
//
//  override def toString: String = blackStrands.map(_.round).toString.substring(3)
//}
//
//class StrandDiagramSpan(val ring: Z2PolynomialRing, val orangeStrands: Set[VariableStrand]) {
//  val zero: StrandDiagramSpanElement =
//    new StrandDiagramSpanElement(this, Map.empty[StrandDiagram, Z2PolynomialRing.Element])
//}
//
//class StrandDiagramSpanElement(val module: StrandDiagramSpan, _terms: Map[StrandDiagram, Z2PolynomialRing.Element]) {
//  val terms: Map[StrandDiagram, Z2PolynomialRing.Element] = _terms.filter(_._2 != module.ring.zero)
//
//  def toAMinusElement(algebra: AMinus): AMinus.Element = {
//    var result = algebra.zero
//    for ((g, c) <- terms) {
//      result += c *: g.toAMinusGenerator(algebra).toElement
//    }
//    result
//  }
//
//  def dPlus: StrandDiagramSpanElement = {
//    var result = module.zero
//    for ((g,c) <- terms) {
//      result += c *: g.dPlus
//    }
//    result
//  }
//
//  def dMinus: StrandDiagramSpanElement = {
//    var result = module.zero
//    for ((g,c) <- terms) {
//      result += c *: g.dMinus
//    }
//    result
//  }
//
//  def +(other: StrandDiagramSpanElement): StrandDiagramSpanElement = {
//    assert (this.module == other.module)
//    new StrandDiagramSpanElement(this.module, this.terms |+| other.terms)
//  }
//
//  def *:(scalar: Z2PolynomialRing.Element): StrandDiagramSpanElement =
//    new StrandDiagramSpanElement(module, terms.view.mapValues(scalar * _).toMap)
//
//  override def equals(other: Any): Boolean = other match {
//    case other: StrandDiagramSpanElement => this.module == other.module && this.terms == other.terms
//    case _ => false
//  }
//
//  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
//
//  override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
//}
//
//object StrandDiagramSpanElement {
//  implicit val strandDiagramSpanElementSemigroup: Semigroup[StrandDiagramSpanElement] =
//    Semigroup.instance((a, b) => a + b)
//}
