package modules

import algebras.Sign.{Negative, Positive}
import algebras.{AMinus, Z2PolynomialRing}
import tangles.ETangle

class ECTMinus(val tangle: ETangle) {
  import tangle._
  val leftAlgebra: AMinus = new AMinus(leftSigns)
  val rightAlgebra: AMinus = new AMinus(rightSigns)
  val ring: Z2PolynomialRing =
    new Z2PolynomialRing((0 until numLeftVars + numMidVars + numRightVars).map(i => s"u$i"))

  def numLeftVars: Int = leftSigns.count(_ == Positive)
  def numMidVars: Int = signs.count(_ == Negative)
  def numRightVars: Int = rightSigns.count(_ == Positive)
}
