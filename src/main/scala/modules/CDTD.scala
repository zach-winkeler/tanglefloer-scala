package modules

import scala.language.implicitConversions
import algebras.Sign.{Negative, Positive}
import algebras.TensorAlgebra.AMinusGeneratorExtensions
import algebras.{AMinus, Z2PolynomialRing}
import modules.Module.{Element, Generator, TensorElement}
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
      val (newS1, newS2) = cross(s1, s2)
      var coefficient = g.module.ring.one
      for (o <- orangeStrands if (o crosses newS1) && (o crosses newS2)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newStrands = (g.label) -- List(s1, s2) ++ List(newS1, newS2)
      result += coefficient *: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newStrands).toElement
    }
    result
  }

  def deltaLeft(g: Module.Generator[TypeDD[Set[Strand]],Set[Strand]], orangeStrands: Set[VariableStrand]):
  TensorElement[TypeDD[Set[Strand]],Set[Strand]] = {
    var result: TensorElement[TypeDD[Set[Strand]],Set[Strand]] = (g.module).zero
    val l = g.leftIdempotent
    // case 1
    for (s1 <- l.strands; s2 <- l.strands if (s1 endsBelow s2) && !(s1 crosses s2)) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- l.strands if (b endsBetween (s1,s2)) && ((b crosses s1) || (b crosses s2))) {
        coefficient *= g.module.ring.zero
      }
      for (b <- (g.label) if (b startsBetweenEnds (s1, s2))) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- l.algebra.orangeStrands if (o endsBetween (s1,s2)) && ((o crosses s1) || (o crosses s2))) {
        if (o.sign == Positive) {
          coefficient *= g.module.leftScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- orangeStrands if o startsBetweenEnds (s1, s2)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newLStrands = l.strands -- List(s1, s2) ++ tuple2ToIndexedSeq(cross(s1, s2))
      result += coefficient *: (l.algebra.gen(newLStrands) <*>: g)
    }
    // case 2
    for (s1 <- (g.label); s2 <- (g.label) if (s1 startsBelow s2) && (s1 crosses s2)) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- l.strands if b endsBetweenStarts (s1,s2)) {
        coefficient *= g.module.ring.zero
      }
      for (b <- (g.label) if (b startsBetween (s1, s2)) && ((b crosses s1) ^ (b crosses s2))) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- l.algebra.orangeStrands if o endsBetweenStarts (s1,s2)) {
        if (o.sign == Positive) {
          coefficient *= g.module.leftScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- orangeStrands if (o startsBetween (s1, s2)) && ((o crosses s1) ^ (o crosses s2))) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newGStrands = (g.label) -- List(s1, s2) ++ tuple2ToIndexedSeq(uncross(s1, s2))
      result += coefficient *: (l <*>: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands))
    }
    // case 3
    for (s1 <- l.strands; s2 <- (g.label) if s1 endsBelowStart s2) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- l.strands if (b startsBelow s1) && (b endsAbove s1) && (b endsBelowStart s2)) {
        coefficient *= g.module.ring.zero
      }
      for (b <- (g.label) if (b startsAboveEnd s1) && (b startsBelow s2) && (b endsBelow s2)) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- l.algebra.orangeStrands if (o endsAbove s1) && (o endsBelowStart s2) && (o crosses s1)) {
        if (o.sign == Positive) {
          coefficient *= g.module.leftScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- orangeStrands if (o startsAboveEnd s1) && (o startsBelow s2) && !(o crosses s2)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newLStrands = l.strands - s1 + Strand(s1.start, s2.start)
      val newGStrands = (g.label) - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (l.algebra.gen(newLStrands) <*>: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands))
    }
    // case 4
    for (s1 <- l.strands; s2 <- (g.label) if s1 endsAboveStart s2) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- l.strands if (b endsBelow s1) && (b endsAboveStart s2) && (b crosses s1)) {
        coefficient *= g.module.ring.zero
      }
      for (b <- (g.label) if (b startsBelowEnd s1) && (b startsAbove s2) && !(b crosses s2)) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- l.algebra.orangeStrands if (o endsBelow s1) && (o endsAboveStart s2) && (o crosses s1)) {
        if (o.sign == Positive) {
          coefficient *= g.module.leftScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- orangeStrands if (o startsBelowEnd s1) && (o startsAbove s2) && !(o crosses s2)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newLStrands = l.strands - s1 + Strand(s1.start, s2.start)
      val newGStrands = (g.label) - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (l.algebra.gen(newLStrands) <*>: g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands))
    }
    result
  }

  def deltaRight(g: Module.Generator[TypeDD[Set[Strand]],Set[Strand]], orangeStrands: Set[VariableStrand]):
  TensorElement[TypeDD[Set[Strand]],Set[Strand]] = {
    var result: TensorElement[TypeDD[Set[Strand]],Set[Strand]] = (g.module).zero
    val r = g.rightIdempotent
    // case 1
    for (s1 <- (r.strands); s2 <- (r.strands) if (s1 startsBelow s2) && !(s1 crosses s2)) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- (g.label) if b endsBetweenStarts (s1,s2)) {
        coefficient *= g.module.ring.zero
      }
      for (b <- r.strands if (b startsBetween (s1, s2)) && ((b crosses s1) || (b crosses s2))) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- orangeStrands if o endsBetweenStarts (s1,s2)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- r.algebra.orangeStrands if (o startsBetween (s1, s2)) && ((o crosses s1) || (o crosses s2))) {
        if (o.sign == Positive) {
          coefficient *= g.module.rightScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newRStrands = r.strands -- List(s1, s2) ++ tuple2ToIndexedSeq(cross(s1, s2))
      result += coefficient *: (g :<*> r.algebra.gen(newRStrands))
    }
    // case 2
    for (s1 <- (g.label); s2 <- (g.label) if (s1 endsBelow s2) && (s1 crosses s2)) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- (g.label) if (b endsBetween (s1,s2)) && ((b crosses s1) ^ (b crosses s2))) {
        coefficient *= g.module.ring.zero
      }
      for (b <- r.strands if b startsBetweenEnds (s1,s2)) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- orangeStrands if (o endsBetween (s1,s2)) && ((o crosses s1) ^ (o crosses s2))) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- r.algebra.orangeStrands if o startsBetweenEnds (s1,s2)) {
        if (o.sign == Positive) {
          coefficient *= g.module.rightScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newGStrands = (g.label) -- List(s1, s2) ++ tuple2ToIndexedSeq(uncross(s1, s2))
      result += coefficient *: (g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands) :<*> r)
    }
    // case 3
    for (s1 <- (g.label); s2 <- r.strands if s1 endsAboveStart s2) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- (g.label) if (b startsBelow s1) && (b endsBelow s1) && (b endsAboveStart s2)) {
        coefficient *= g.module.ring.zero
      }
      for (b <- r.strands if (b startsBelowEnd s1) && (b startsAbove s2) && (b endsBelow s2)) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- orangeStrands if (o startsBelow s1) && (o endsBelow s1) && (o endsAboveStart s2)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- r.algebra.orangeStrands if (o startsBelowEnd s1) && (o startsAbove s2) && (o endsBelow s2)) {
        if (o.sign == Positive) {
          coefficient *= g.module.rightScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newGStrands = (g.label) - s1 + Strand(s1.start, s2.start)
      val newRStrands = r.strands - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands) :<*> r.algebra.gen(newRStrands))
    }
    // case 4
    for (s1 <- (g.label); s2 <- r.strands if s1 endsBelowStart s2) {
      var coefficient = g.module.ring.one
      // black strands
      for (b <- (g.label) if (b startsAbove s1) && (b endsAbove s1) && (b endsBelowStart s2)) {
        coefficient *= g.module.ring.zero
      }
      for (b <- r.strands if (b startsAboveEnd s1) && (b startsBelow s2) && (b endsAbove s2)) {
        coefficient *= g.module.ring.zero
      }
      // orange strands
      for (o <- orangeStrands if (o startsAbove s1) && (o endsAbove s1) && (o endsBelowStart s2)) {
        if (o.sign == Negative) {
          coefficient *= o.variable
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      for (o <- r.algebra.orangeStrands if (o startsAboveEnd s1) && (o startsBelow s2) && (o endsAbove s2)) {
        if (o.sign == Positive) {
          coefficient *= g.module.rightScalarAction(o.variable)
        } else {
          coefficient *= g.module.ring.zero
        }
      }
      val newGStrands = (g.label) - s1 + Strand(s1.start, s2.start)
      val newRStrands = r.strands - s2 + Strand(s1.end, s2.end)
      result += coefficient *: (g.module.asInstanceOf[TypeDD[Set[Strand]]].gen(newGStrands) :<*> r.algebra.gen(newRStrands))
    }
    result
  }
}
