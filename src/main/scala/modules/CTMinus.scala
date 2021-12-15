package modules

import scala.language.implicitConversions

import algebras.Sign.Positive
import algebras.{AMinus, Z2PolynomialRing}
import modules.Module.Generator
import tangles.StrandUtils._
import tangles.{ETangle, StrandDiagram, StrandDiagramSpan}
import utilities.Functions.partialBijections

object CTMinus {
  def typeAA(etangle: ETangle): TypeAA = {
    val leftAlgebra = new AMinus(etangle.middleSigns)
    val rightAlgebra = new AMinus(etangle.rightSigns)
    val leftScalarAction = new Z2PolynomialRing.Morphism(leftAlgebra.ring, rightAlgebra.ring,
      etangle.rightStrands.filter(_.sign == Positive).map(
        o => s"u${leftAlgebra.positives.indexOf(o.start.toInt)}"
          -> s"u${rightAlgebra.positives.indexOf(o.end.toInt)}"
      ).toMap)
    var result = new TypeAA(rightAlgebra.ring, leftAlgebra, rightAlgebra,
      leftScalarAction, Z2PolynomialRing.Morphism.identity(rightAlgebra.ring))()

    val orangeStrands = etangle.rightStrands.map(o => VariableStrand(o.start, o.end, o.sign,
      if (o.sign == Positive)
        leftAlgebra.ring.vars(leftAlgebra.positives.indexOf(o.start.toInt))
      else
        leftAlgebra.ring.zero
    ))
    val module = new StrandDiagramSpan(rightAlgebra.ring, orangeStrands)
    val genDiagrams = partialBijections(etangle.middlePoints, etangle.rightPoints).map(s =>
      new StrandDiagram(module, s.map(t => Strand(t._1, t._2))))
    val gens = genDiagrams.map(g => new Generator[TypeAA](
      result, g, leftAlgebra.gen(g.leftIdempotentStrands), rightAlgebra.gen(g.rightIdempotentStrands)))
    for (g <- gens) {
      result.addGenerator(g)
    }
    for (g <- gens; l <- leftAlgebra.gens) {
      // result.addStructureMap(l <*>: g, )
    }
    result
  }
}
