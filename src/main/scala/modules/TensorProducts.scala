package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{EdgeLabel, Element, Generator}
import scalax.collection.edge.LkDiEdge

object TensorProducts {
  implicit class AAGeneratorTensorProducts[L1](base: Generator[TypeAA[L1],L1]) {
    def <*>[L2](other: Generator[TypeDD[L2],L2])(implicit module: TypeAD[(L1,L2)]): Element[TypeAD[(L1,L2)],(L1,L2)] = {
      if (base.rightIdempotent == other.leftIdempotent) {
        new Generator(module, (base.label, other.label), base.leftIdempotent, other.rightIdempotent).toElement
      } else {
        module.zero
      }
    }
  }

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

  implicit class ADGeneratorTensorProducts[L1](base: Generator[TypeAD[L1], L1]) {
    def <*>[L2](other: Generator[TypeAA[L2],L2])(implicit module: TypeAA[(L1,L2)]): Element[TypeAA[(L1,L2)],(L1,L2)] = {
      if (base.rightIdempotent == other.leftIdempotent) {
        new Generator(module, (base.label, other.label), base.leftIdempotent, other.rightIdempotent).toElement
      } else {
        module.zero
      }
    }
  }

  // outputs (coefficient, right, target)
  implicit class DDGeneratorDeltaMaps[L1](g: Generator[TypeDD[L1],L1]) {
    def deltaOutputtingLeft(leftFactors: IndexedSeq[AMinus.Generator]):
    Set[(Z2PolynomialRing.Element, TensorAlgebra.Generator, Generator[TypeDD[L1],L1])] = {
      if (leftFactors.isEmpty) {
        Set((g.module.ring.one, g.module.rightTensorAlgebra.oneGen, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if l.factors(0) == leftFactors.head;
             (coefficient, right, target) <- t.value.deltaOutputtingLeft(leftFactors.tail)
             if right <*> r != g.module.rightTensorAlgebra.zero) yield {
          (c * coefficient, (right <*> r).forceGen, target)
        }
      }
    }

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

  // outputs (left, coefficient, target)
  implicit class ADGeneratorDeltaMaps[L1](g: Generator[TypeAD[L1],L1]) {
    def deltaOutputtingRight(rightFactors: IndexedSeq[AMinus.Generator]):
    Set[(TensorAlgebra.Generator, Z2PolynomialRing.Element, Generator[TypeAD[L1],L1])] = {
      if (rightFactors.isEmpty) {
        Set((g.module.leftTensorAlgebra.oneGen, g.module.ring.one, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if r.factors(0) == rightFactors.last;
             (left, coefficient, target) <- t.value.deltaOutputtingRight(rightFactors.dropRight(1))
             if left <*> l != g.module.leftTensorAlgebra.zero) yield {
          ((left <*> l).forceGen, c * coefficient, target)
        }
      }
    }
  }

  implicit class TypeAATensorProducts[L1](base: TypeAA[L1]) {
    def <*>[L2](other: TypeDD[L2]): TypeAD[(L1, L2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeAD[(L1, L2)] = new TypeAD(inLeft.target, base.leftAlgebra, other.rightAlgebra,
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
            val rightProd = rightN.factors.foldLeft(yN.rightIdempotent.toElement) { (prod, g) => prod * g.toElement }
            result.addStructureMap((leftM <*>: (xM <*> xN).forceGen).forceGen, c *: ((yM <*> yN) :<*> rightProd))
          }
        }
      }
      result
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
            val leftProd = leftM.factors.foldRight(yM.leftIdempotent.toElement) { (g, prod) => g.toElement * prod }
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

  implicit class TypeADTensorProducts[L1](base: TypeAD[L1]) {
    def <*>[L2](other: TypeAA[L2]): TypeAA[(L1, L2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeAA[(L1, L2)] = new TypeAA(inLeft.target, base.leftAlgebra, other.rightAlgebra,
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
            result.addStructureMap((leftM <*>: ((xM <*> xN).forceGen :<*> rightN).forceGen).forceGen, c *: (yM <*> yN))
          }
        }
      }
      result
    }
  }
}
