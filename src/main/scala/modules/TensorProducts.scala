package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{EdgeLabel, Element, Generator}
import scalax.collection.edge.LkDiEdge

object TensorProducts {
  implicit class DDGeneratorTensorProducts[L1](base: Generator[TypeDD[L1],L1]) {
    def <*>[L2](other: Generator[TypeAA[L2],L2])(implicit module: TypeDA[(L1,L2)]): Element[TypeDA[(L1,L2)],(L1,L2)] = {
      if (base.rightIdempotent == other.leftIdempotent) {
        new Generator(module, (base.label, other.label), base.leftIdempotent, other.rightIdempotent).toElement
      } else {
        module.zero
      }
    }
  }

  // outputs (left, coefficient, target)
  implicit class DDGeneratorDeltaMaps[L1](g: Generator[TypeDD[L1],L1]) {
    def deltaOutputtingRight(rightFactors: IndexedSeq[AMinus.Generator]):
    Set[(TensorAlgebra.Generator, Z2PolynomialRing.Element, Generator[TypeDD[L1],L1])] = {
      if (rightFactors.isEmpty) {
        Set((g.module.leftTensorAlgebra.oneGen, g.module.ring.one, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if r.factors(0) == rightFactors.last;
             (left, coefficient, target) <- t.value.deltaOutputtingRight(rightFactors.dropRight(1))
             if left.factors.isEmpty || left.leftIdempotent.contains(l.factors(0).rightIdempotent)) yield {
          ((l.factors(0) <*>: left).forceGen, c * coefficient, target)
        }
      }
    }
  }

  implicit class TypeDDTensorProducts[L1](base: TypeDD[L1]) {
    def <*>[L2](other: TypeAA[L2]): TypeDA[(L1, L2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeDA[(L1, L2)] = new TypeDA(inLeft.target, base.leftAlgebra, other.rightAlgebra,
        inLeft.compose(base.leftScalarAction), inRight.compose(other.rightScalarAction))()

      for (gM <- base.gens;
           gN <- other.gens if gM.rightIdempotent == gN.leftIdempotent) {
        result.addGenerator((gM <*> gN).forceGen)
      }
      for (xM <- base.gens;
           xN <- other.gens if xM.rightIdempotent == xN.leftIdempotent) {
        for (LkDiEdge(_, _yN, EdgeLabel(leftN, coefficientN, rightN)) <- xN.outgoing
             if rightN.leftIdempotent.contains(xN.rightIdempotent)) {
          val yN = _yN.value
          for ((leftM, coefficientM, yM) <- xM.deltaOutputtingRight(leftN.factors)) {
            val c = inLeft(coefficientM) * inRight(coefficientN)
            val leftProd = leftM.factors.foldRight(yM.leftIdempotent.toElement) {(g, prod) => g.toElement * prod}
            result.addStructureMap(((xM <*> xN).forceGen :<*> rightN).forceGen, c *: (leftProd <*>: (yM <*> yN)))
          }
        }
      }
      result
    }
  }
}
