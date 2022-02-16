package modules

import scala.language.implicitConversions
import algebras.Sign.{Negative, Positive}
import algebras.TensorAlgebra.AMinusGeneratorExtensions
import algebras.{AMinus, Z2PolynomialRing}
import modules.Module.{Element, Generator, TensorElement}
import tangles.ETangleType.{Cup, Cap}
import tangles.StrandUtils._
import tangles.PartialBijectionUtils._
import tangles.{ETangle, Strand, VariableStrand}
import utilities.Functions.{partialBijections, tuple2ToIndexedSeq}
import utilities.IndexedSeqUtils.IndexedSeqExtensions

object CDTD {
  implicit class TypeDDExtensions(module: TypeDD[Set[Strand]]) {
    def gen(strands: Set[Strand]): Generator[TypeDD[Set[Strand]], Set[Strand]] = {
      new Generator[TypeDD[Set[Strand]],Set[Strand]](module, strands,
        module.leftAlgebra.gen(strands.sourceId).complement, module.rightAlgebra.gen(strands.targetId).complement)
    }
  }

  def from(etangle: ETangle): TypeDD[Set[Strand]] = {
    val middleStrandsOrdered = etangle.leftStrands.toIndexedSeq
    val negatives = middleStrandsOrdered.indicesWhere(_.sign == Negative)
    val middleRing = new Z2PolynomialRing(negatives.indices.map(i => s"u$i"))
    val leftAlgebra = new AMinus(etangle.leftSigns)
    val rightAlgebra = new AMinus(etangle.middleSigns)
    val (leftToLeftMiddle, middleToLeftMiddle) = leftAlgebra.ring.tensorInclusions(middleRing)
    val leftMiddle = leftToLeftMiddle.target
    val (leftMiddleToAll, rightScalarAction) = leftMiddle.tensorInclusions(rightAlgebra.ring)
    val leftScalarAction = leftMiddleToAll.compose(leftToLeftMiddle)
    val middleScalarAction = leftMiddleToAll.compose(middleToLeftMiddle)
    val ring = leftMiddleToAll.target

    val result = new TypeDD[Set[Strand]](ring, leftAlgebra, rightAlgebra, leftScalarAction, rightScalarAction)()

    val orangeStrands = middleStrandsOrdered.map(o => VariableStrand(o.start, o.end, o.sign,
      if (o.sign == Negative) {
        middleScalarAction.apply(middleRing.vars(negatives.indexOf(middleStrandsOrdered.indexOf(o))))
      } else
        middleScalarAction.apply(middleRing.zero)
    )).toSet

    val gens = partialBijections(etangle.leftPoints, etangle.middlePoints).map(pb =>
      pb.map(Strand.fromTuple)).map(strands => result.gen(strands))

    for (g <- gens) {
      result.addGenerator(g)
    }
    for (g <- gens) {
      result.addStructureMap(g, g.leftIdempotent.toElement <*>: dMinus(g, orangeStrands) :<*> g.rightIdempotent.toElement)
      result.addStructureMap(g, deltaLeft(g, orangeStrands) :<*> g.rightIdempotent.toElement)
      result.addStructureMap(g, g.leftIdempotent.toElement  <*>: deltaRight(g, orangeStrands))
    }
    result
  }

  def dMinus(g: Module.Generator[TypeDD[Set[Strand]],Set[Strand]], orangeStrands: Set[VariableStrand]):
  Element[TypeDD[Set[Strand]],Set[Strand]] = {
    var result = (g.module).zero
    for (s1 <- (g.label);
         s2 <- (g.label) if (s1 startsBelow s2) && !(s1 crosses s2)) {
      var coefficient = computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => (s startsBetween (s1,s2)) && (s endsBetween (s1,s2)))
      val newStrands = (g.label).cross(s1, s2)
      result += coefficient *: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newStrands).toElement
    }
    result
  }

  def deltaLeft(g: Module.Generator[TypeDD[Set[Strand]],Set[Strand]],
                orangeStrands: Set[VariableStrand]):
  TensorElement[TypeDD[Set[Strand]],Set[Strand]] = {
    var result: TensorElement[TypeDD[Set[Strand]],Set[Strand]] = (g.module).zero
    val l = g.leftIdempotent
    // case 1
    for (s1 <- l.strands; s2 <- l.strands if (s1 endsBelow s2) && !(s1 crosses s2)) {
      var coefficient = g.module.ring.one
      coefficient *= g.module.leftScalarAction(computeCoefficient(l.algebra.ring, l.strands, l.algebra.orangeStrands,
        s => (s endsBetween (s1,s2)) && ((s crosses s1) || (s crosses s2))))
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => s startsBetweenEnds (s1, s2))
      val newLStrands = l.strands.cross(s1, s2)
      result += coefficient *: (l.algebra.gen(newLStrands) <*>: g)
    }
    // case 2
    for (s1 <- (g.label); s2 <- (g.label) if (s1 startsBelow s2) && (s1 crosses s2)) {
      var coefficient = g.module.ring.one
      coefficient *= g.module.leftScalarAction(computeCoefficient(l.algebra.ring, l.strands, l.algebra.orangeStrands,
        s => s endsBetweenStarts (s1,s2)))
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => (s startsBetween (s1, s2)) && ((s crosses s1) ^ (s crosses s2)))
      val newGStrands = (g.label).uncross(s1, s2)
      result += coefficient *: (l <*>: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands))
    }
    // case 3
    for (s1 <- l.strands; s2 <- (g.label) if s1 endsBelowStart s2) {
      var coefficient = g.module.ring.one
      coefficient *= g.module.leftScalarAction(computeCoefficient(l.algebra.ring, l.strands, l.algebra.orangeStrands,
        s => (s startsBelow s1) && (s endsAbove s1) && (s endsBelowStart s2)))
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => (s startsAboveEnd s1) && (s startsBelow s2) && (s endsBelow s2))
      val newLStrands = l.strands - s1 + Strand(s1.start, s2.start)
      val newGStrands = (g.label) - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (l.algebra.gen(newLStrands) <*>: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands))
    }
    // case 4
    for (s1 <- l.strands; s2 <- (g.label) if s1 endsAboveStart s2) {
      var coefficient = g.module.ring.one
      coefficient *= g.module.leftScalarAction(computeCoefficient(l.algebra.ring, l.strands, l.algebra.orangeStrands,
        s => (s endsBelow s1) && (s endsAboveStart s2) && (s crosses s1)))
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => (s startsBelowEnd s1) && (s startsAbove s2) && !(s crosses s2))
      val newLStrands = l.strands - s1 + Strand(s1.start, s2.start)
      val newGStrands = (g.label) - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (l.algebra.gen(newLStrands) <*>: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands))
    }
    result
  }

  def deltaRight(g: Module.Generator[TypeDD[Set[Strand]],Set[Strand]],
                 orangeStrands: Set[VariableStrand]):
  TensorElement[TypeDD[Set[Strand]],Set[Strand]] = {
    var result: TensorElement[TypeDD[Set[Strand]],Set[Strand]] = (g.module).zero
    val r = g.rightIdempotent
    // case 1
    for (s1 <- (r.strands); s2 <- (r.strands) if (s1 startsBelow s2) && !(s1 crosses s2)) {
      var coefficient = g.module.ring.one
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => s endsBetweenStarts (s1, s2))
      coefficient *= g.module.rightScalarAction(computeCoefficient(r.algebra.ring, r.strands, r.algebra.orangeStrands,
        s => (s startsBetween (s1, s2)) && ((s crosses s1) || (s crosses s2))))
      val newRStrands = r.strands.cross(s1, s2)
      result += coefficient *: (g :<*> r.algebra.gen(newRStrands))
    }
    // case 2
    for (s1 <- (g.label); s2 <- (g.label) if (s1 endsBelow s2) && (s1 crosses s2)) {
      var coefficient = g.module.ring.one
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => (s endsBetween (s1,s2)) && ((s crosses s1) ^ (s crosses s2)))
      coefficient *= g.module.rightScalarAction(computeCoefficient(r.algebra.ring, r.strands, r.algebra.orangeStrands,
        s => s startsBetweenEnds (s1,s2)))
      val newGStrands = (g.label).uncross(s1, s2)
      result += coefficient *: (g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands) :<*> r)
    }
    // case 3
    for (s1 <- (g.label); s2 <- r.strands if s1 endsAboveStart s2) {
      var coefficient = g.module.ring.one
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => (s startsBelow s1) && (s endsBelow s1) && (s endsAboveStart s2))
      coefficient *= g.module.rightScalarAction(computeCoefficient(r.algebra.ring, r.strands, r.algebra.orangeStrands,
        s => (s startsBelowEnd s1) && (s startsAbove s2) && (s endsBelow s2)))
      val newGStrands = (g.label) - s1 + Strand(s1.start, s2.start)
      val newRStrands = r.strands - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands) :<*> r.algebra.gen(newRStrands))
    }
    // case 4
    for (s1 <- (g.label); s2 <- r.strands if s1 endsBelowStart s2) {
      var coefficient = g.module.ring.one
      coefficient *= computeCoefficient(g.module.ring, g.label, orangeStrands,
        s => (s startsAbove s1) && (s endsAbove s1) && (s endsBelowStart s2))
      coefficient *= g.module.rightScalarAction(computeCoefficient(r.algebra.ring, r.strands, r.algebra.orangeStrands,
        s => (s startsAboveEnd s1) && (s startsBelow s2) && (s endsAbove s2)))
      val newGStrands = (g.label) - s1 + Strand(s1.start, s2.start)
      val newRStrands = r.strands - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands) :<*> r.algebra.gen(newRStrands))
    }
    result
  }
}
