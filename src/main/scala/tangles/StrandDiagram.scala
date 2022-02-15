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
  override def toString: String =
    if (start % 1 == 0f) {
      start.round.toString + " -> " + end.round.toString
    } else {
      start.toString + " -> " + end.toString
    }
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
