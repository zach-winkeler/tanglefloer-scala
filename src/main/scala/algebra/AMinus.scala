package algebra

import algebra.Sign._
import utilities.VectorUtils.VectorImprovements

object Sign extends Enumeration {
  type Sign = Value
  val Positive: Value = Value(1)
  val Negative: Value = Value(-1)
}

class AMinus(val signSequence: Vector[Sign]) {
  val positives: Vector[Int] = this.signSequence.indicesWhere(_ == Sign.Positive)
  val ring = new Z2PolynomialRing(this.positives.map(i => s"U$i").toSet)
}
