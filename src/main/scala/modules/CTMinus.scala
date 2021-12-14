package modules

import algebras.Sign.Positive
import algebras.{AMinus, Z2PolynomialRing}
import tangles.ETangle

object CTMinus {
  def typeAA(etangle: ETangle): TypeAA = {
    val leftAlgebra = new AMinus(etangle.middleSigns)
    val rightAlgebra = new AMinus(etangle.rightSigns)
    var result = new TypeAA(rightAlgebra.ring, leftAlgebra, rightAlgebra,
      ???, Z2PolynomialRing.Morphism.identity(rightAlgebra.ring))()
    result
  }
}
