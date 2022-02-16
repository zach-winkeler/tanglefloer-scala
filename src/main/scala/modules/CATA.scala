package modules

import scala.language.implicitConversions
import algebras.Sign.Positive
import algebras.{AMinus, Z2PolynomialRing}
import modules.Module.{Element, Generator}
import tangles.StrandUtils._
import tangles.PartialBijectionUtils._
import tangles.{ETangle, Strand, VariableStrand}
import utilities.Functions.{partialBijections, tuple2ToIndexedSeq}

object CATA {
  implicit class TypeAAExtensions(module: TypeAA[Set[Strand]]) {
    def gen(strands: Set[Strand]): Generator[TypeAA[Set[Strand]], Set[Strand]] = {
      new Generator[TypeAA[Set[Strand]],Set[Strand]](module, strands,
        module.leftAlgebra.gen(strands.sourceId), module.rightAlgebra.gen(strands.targetId))
    }
  }

  def from(etangle: ETangle): TypeAA[Set[Strand]] = {
    val leftAlgebra = new AMinus(etangle.middleSigns)
    val rightAlgebra = new AMinus(etangle.rightSigns)
    val rightScalarAction = new Z2PolynomialRing.Morphism(rightAlgebra.ring, leftAlgebra.ring,
      (for (o <- etangle.rightStrands.filter(_.sign == Positive)) yield
        (s"u${rightAlgebra.positives.indexOf(o.end.toInt)}"
          -> s"u${leftAlgebra.positives.indexOf(o.start.toInt)}")).toMap
    )
    val result = new TypeAA[Set[Strand]](leftAlgebra.ring, leftAlgebra, rightAlgebra,
      Z2PolynomialRing.Morphism.identity(leftAlgebra.ring), rightScalarAction)()

    val orangeStrands = etangle.rightStrands.map(o => VariableStrand(o.start, o.end, o.sign,
      if (o.sign == Positive) {
        leftAlgebra.ring.vars(leftAlgebra.positives.indexOf(o.start.toInt))
      } else
        leftAlgebra.ring.zero
    ))

    val gens = partialBijections(etangle.middlePoints, etangle.rightPoints).map(pb =>
      pb.map(Strand.fromTuple)).map(strands => result.gen(strands))

    for (g <- gens) {
      result.addGenerator(g)
    }
    for (g <- gens) {
      result.addStructureMap(g, m1(g, orangeStrands))
    }
    for (g <- gens; l <- leftAlgebra.gens) {
      if (l.rightIdempotent == g.leftIdempotent) {
        result.addStructureMap((l <*>: g).forceGen, m2(l, g, orangeStrands))
      }
    }
    for (g <- gens; r <- rightAlgebra.gens) {
      if (g.rightIdempotent == r.leftIdempotent) {
        result.addStructureMap((g :<*> r).forceGen, m2(g, r, orangeStrands))
      }
    }
    result
  }

  def m1(g: Module.Generator[TypeAA[Set[Strand]],Set[Strand]], orangeStrands: Set[VariableStrand]):
  Element[TypeAA[Set[Strand]],Set[Strand]] = {
    var result = g.module.zero
    for (s1 <- (g.label);
         s2 <- (g.label) if (s1 startsBelow s2) && (s1 crosses s2)) {
      var coefficient = g.module.ring.one
      for (s <- (g.label).map(_.toVariableStrand(g.module.ring.zero)) | orangeStrands
           if (s startsBetween (s1,s2)) && (s endsBetween (s2,s1))) {
        coefficient *= s.variable
      }
      val newStrands = (g.label) -- List(s1, s2) ++ tuple2ToIndexedSeq(uncross(s1, s2))
      result += coefficient *: g.module.asInstanceOf[TypeAA[Set[Strand]]].gen(newStrands).toElement
    }
    result.asInstanceOf[Element[TypeAA[Set[Strand]],Set[Strand]]]
  }

  def m2(l: AMinus.Generator, g: Module.Generator[TypeAA[Set[Strand]],Set[Strand]], orangeStrands: Set[VariableStrand]):
  Element[TypeAA[Set[Strand]],Set[Strand]] = {
    var coefficient = g.module.ring.one
    var newHalfStrands = Set.empty[(Strand, Strand)]
    var newStrands = Set.empty[Strand]
    for (s1 <- l.strands) {
      (g.label).find(_.start == s1.end) match {
        case Some(s2) =>
          for ((s3, s4) <- newHalfStrands) {
            if ((s1 crosses s3) && (s2 crosses s4)) {
              coefficient *= g.module.ring.zero
            }
          }
          newHalfStrands = newHalfStrands.incl((s1, s2))
          newStrands += s1.start -> s2.end
          for (o <- orangeStrands if (Strand(o.start, o.start) crosses s1) && (o crosses s2)) {
            coefficient *= o.variable
          }
        case None =>
          coefficient *= g.module.ring.zero
      }
    }
    coefficient *: g.module.asInstanceOf[TypeAA[Set[Strand]]].gen(newStrands).toElement
  }

  def m2(g: Module.Generator[TypeAA[Set[Strand]],Set[Strand]], r: AMinus.Generator, orangeStrands: Set[VariableStrand]):
  Element[TypeAA[Set[Strand]],Set[Strand]] = {
    var coefficient = g.module.ring.one
    var newHalfStrands = Set.empty[(Strand, Strand)]
    var newStrands = Set.empty[Strand]
    for (s1 <- (g.label)) {
      r.strands.find(_.start == s1.end) match {
        case Some(s2) =>
          for ((s3, s4) <- newHalfStrands) {
            if ((s1 crosses s3) && (s2 crosses s4)) {
              coefficient *= g.module.ring.zero
            }
          }
          newHalfStrands = newHalfStrands.incl((s1, s2))
          newStrands += s1.start -> s2.end
          for (o <- orangeStrands if (o crosses s1) && (Strand(o.end, o.end) crosses s2)) {
            coefficient *= o.variable
          }
        case None =>
          coefficient *= g.module.ring.zero
      }
    }
    coefficient *: g.module.asInstanceOf[TypeAA[Set[Strand]]].gen(newStrands).toElement
  }
}
