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

  implicit class DAGeneratorTensorProducts[L1](base: Generator[TypeDA[L1], L1]) {
    def <*>[L2](other: Generator[TypeDA[L2],L2])(implicit module: TypeDA[(L1,L2)]): Element[TypeDA[(L1,L2)],(L1,L2)] = {
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
             if l <*> left != g.module.leftTensorAlgebra.zero) yield {
          ((l <*> left).forceGen, c * coefficient, target)
        }
      }
    }
  }

  // outputs (coefficient, right, target)
  implicit class DAGeneratorDeltaMaps[L1](g: Generator[TypeDA[L1],L1]) {
    def deltaOutputtingLeft(leftFactors: IndexedSeq[AMinus.Generator]):
    Set[(Z2PolynomialRing.Element, TensorAlgebra.Generator, Generator[TypeDA[L1],L1])] = {
      if (leftFactors.isEmpty) {
        Set((g.module.ring.one, g.module.rightTensorAlgebra.oneGen, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if l.factors(0) == leftFactors.head;
             (coefficient, right, target) <- t.value.deltaOutputtingLeft(leftFactors.tail)
             if r <*> right != g.module.rightTensorAlgebra.zero) yield {
          (c * coefficient, (r <*> right).forceGen, target)
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
        for (LkDiEdge(_, _yN, EdgeLabel(leftN, coefficientN, rightN)) <- xN.outgoing) {
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

  implicit class TypeDATensorProducts[L1](base: TypeDA[L1]) {
    def <*>[L2](other: TypeDA[L2]): TypeDA[(L1, L2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeDA[(L1, L2)] = new TypeDA(inLeft.target, base.leftAlgebra, other.rightAlgebra,
        inLeft.compose(base.leftScalarAction), inRight.compose(other.rightScalarAction))()

      for (gM <- base.gens;
           gN <- other.gens if gM.rightIdempotent == gN.leftIdempotent) {
        result.addGenerator((gM <*> gN).forceGen)
      }
      for (xM <- base.gens;
           xN <- other.gens if xM.rightIdempotent == xN.leftIdempotent) {
        for (LkDiEdge(_, _yM, EdgeLabel(leftM, coefficientM, rightM)) <- xM.outgoing) {
          val yM = _yM.value
          for ((coefficientN, rightN, yN) <- xN.deltaOutputtingLeft(rightM.factors)) {
            val c = inLeft(coefficientM) * inRight(coefficientN)
            result.addStructureMap(((xM <*> xN).forceGen :<*> rightN).forceGen, c *: (leftM.toElement <*>: (yM <*> yN)))
          }
        }
      }
      result
    }
  }
}
