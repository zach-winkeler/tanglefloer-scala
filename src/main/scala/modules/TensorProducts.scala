package modules

import algebras.{AMinus, TensorAlgebra, Z2PolynomialRing}
import modules.Module.{EdgeLabel, Element, Generator}
import scalax.collection.edge.LkDiEdge

object TensorProducts {
  implicit class AAGeneratorTensorProducts[K1](base: Generator[TypeAA[K1],K1]) {
    def <*>[K2](other: Generator[TypeDD[K2],K2])(implicit module: TypeAD[(K1,K2)]): Element[TypeAD[(K1,K2)],(K1,K2)] = {
      if (base.rightIdempotent == other.leftIdempotent) {
        new Generator(module, (base.key, other.key), base.leftIdempotent, other.rightIdempotent).toElement
      } else {
        module.zero
      }
    }
  }

  implicit class DDGeneratorTensorProducts[K1](base: Generator[TypeDD[K1],K1]) {
    def <*>[K2](other: Generator[TypeAA[K2],K2])(implicit module: TypeDA[(K1,K2)]): Element[TypeDA[(K1,K2)],(K1,K2)] = {
      if (base.rightIdempotent == other.leftIdempotent) {
        new Generator(module, (base.key, other.key), base.leftIdempotent, other.rightIdempotent).toElement
      } else {
        module.zero
      }
    }
  }

  implicit class DAGeneratorTensorProducts[K1](base: Generator[TypeDA[K1], K1]) {
    def <*>[K2](other: Generator[TypeDA[K2],K2])(implicit module: TypeDA[(K1,K2)]): Element[TypeDA[(K1,K2)],(K1,K2)] = {
      if (base.rightIdempotent == other.leftIdempotent) {
        new Generator(module, (base.key, other.key), base.leftIdempotent, other.rightIdempotent).toElement
      } else {
        module.zero
      }
    }
  }

  implicit class ADGeneratorTensorProducts[K1](base: Generator[TypeAD[K1], K1]) {
    def <*>[K2](other: Generator[TypeAA[K2],K2])(implicit module: TypeAA[(K1,K2)]): Element[TypeAA[(K1,K2)],(K1,K2)] = {
      if (base.rightIdempotent == other.leftIdempotent) {
        new Generator(module, (base.key, other.key), base.leftIdempotent, other.rightIdempotent).toElement
      } else {
        module.zero
      }
    }
  }

  // outputs (coefficient, right, target)
  implicit class DDGeneratorDeltaMaps[K1](g: Generator[TypeDD[K1],K1]) {
    def deltaOutputtingLeft(leftFactors: IndexedSeq[AMinus.Generator]):
    Set[(Z2PolynomialRing.Element, TensorAlgebra.Generator, Generator[TypeDD[K1],K1])] = {
      if (leftFactors.isEmpty) {
        Set((g.module.ring.one, g.module.rightTensorAlgebra.oneGen, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if l.factors(0) == leftFactors.head;
             (coefficient, right, target) <- g.module.gen(t.value).deltaOutputtingLeft(leftFactors.tail)
             if right <*> r != g.module.rightTensorAlgebra.zero) yield {
          (c * coefficient, (right <*> r).forceGen, target)
        }
      }
    }

    def deltaOutputtingRight(rightFactors: IndexedSeq[AMinus.Generator]):
    Set[(TensorAlgebra.Generator, Z2PolynomialRing.Element, Generator[TypeDD[K1],K1])] = {
      if (rightFactors.isEmpty) {
        Set((g.module.leftTensorAlgebra.oneGen, g.module.ring.one, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if r.factors(0) == rightFactors.last;
             (left, coefficient, target) <- g.module.gen(t.value).deltaOutputtingRight(rightFactors.dropRight(1))
             if l <*> left != g.module.leftTensorAlgebra.zero) yield {
          ((l <*> left).forceGen, c * coefficient, target)
        }
      }
    }
  }

  // outputs (coefficient, right, target)
  implicit class DAGeneratorDeltaMaps[K1](g: Generator[TypeDA[K1],K1]) {
    def deltaOutputtingLeft(leftFactors: IndexedSeq[AMinus.Generator]):
    Set[(Z2PolynomialRing.Element, TensorAlgebra.Generator, Generator[TypeDA[K1],K1])] = {
      if (leftFactors.isEmpty) {
        Set((g.module.ring.one, g.module.rightTensorAlgebra.oneGen, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if l.factors(0) == leftFactors.head;
             (coefficient, right, target) <- g.module.gen(t.value).deltaOutputtingLeft(leftFactors.tail)
             if r <*> right != g.module.rightTensorAlgebra.zero) yield {
          (c * coefficient, (r <*> right).forceGen, target)
        }
      }
    }
  }

  // outputs (left, coefficient, target)
  implicit class ADGeneratorDeltaMaps[K1](g: Generator[TypeAD[K1],K1]) {
    def deltaOutputtingRight(rightFactors: IndexedSeq[AMinus.Generator]):
    Set[(TensorAlgebra.Generator, Z2PolynomialRing.Element, Generator[TypeAD[K1],K1])] = {
      if (rightFactors.isEmpty) {
        Set((g.module.leftTensorAlgebra.oneGen, g.module.ring.one, g))
      } else {
        for (LkDiEdge(_, t, EdgeLabel(l, c, r)) <- g.outgoing if r.factors(0) == rightFactors.last;
             (left, coefficient, target) <- g.module.gen(t.value).deltaOutputtingRight(rightFactors.dropRight(1))
             if left <*> l != g.module.leftTensorAlgebra.zero) yield {
          ((left <*> l).forceGen, c * coefficient, target)
        }
      }
    }
  }

  implicit class TypeAATensorProducts[K1](base: TypeAA[K1]) {
    def <*>[K2](other: TypeDD[K2]): TypeAD[(K1, K2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeAD[(K1, K2)] = new TypeAD(inLeft.target, base.leftAlgebra, other.rightAlgebra,
        inLeft.compose(base.leftScalarAction), inRight.compose(other.rightScalarAction))()

      for (gM <- base.gens;
           gN <- other.gens if gM.rightIdempotent == gN.leftIdempotent) {
        result.addGenerator((gM <*> gN).forceGen.label)
      }
      for (xM <- base.gens;
           xN <- other.gens if xM.rightIdempotent == xN.leftIdempotent) {
        for (LkDiEdge(_, _yM, EdgeLabel(leftM, coefficientM, rightM)) <- xM.outgoing) {
          val yM = base.gen(_yM.value)
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

  implicit class TypeDDTensorProducts[K1](base: TypeDD[K1]) {
    def <*>[K2](other: TypeAA[K2]): TypeDA[(K1, K2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeDA[(K1, K2)] = new TypeDA(inLeft.target, base.leftAlgebra, other.rightAlgebra,
        inLeft.compose(base.leftScalarAction), inRight.compose(other.rightScalarAction))()

      for (gM <- base.gens;
           gN <- other.gens if gM.rightIdempotent == gN.leftIdempotent) {
        result.addGenerator((gM <*> gN).forceGen.label)
      }
      for (xM <- base.gens;
           xN <- other.gens if xM.rightIdempotent == xN.leftIdempotent) {
        for (LkDiEdge(_, _yN, EdgeLabel(leftN, coefficientN, rightN)) <- xN.outgoing) {
          val yN = other.gen(_yN.value)
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

  implicit class TypeDATensorProducts[K1](base: TypeDA[K1]) {
    def <*>[K2](other: TypeDA[K2]): TypeDA[(K1, K2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeDA[(K1, K2)] = new TypeDA(inLeft.target, base.leftAlgebra, other.rightAlgebra,
        inLeft.compose(base.leftScalarAction), inRight.compose(other.rightScalarAction))()

      for (gM <- base.gens;
           gN <- other.gens if gM.rightIdempotent == gN.leftIdempotent) {
        result.addGenerator((gM <*> gN).forceGen.label)
      }
      for (xM <- base.gens;
           xN <- other.gens if xM.rightIdempotent == xN.leftIdempotent) {
        for (LkDiEdge(_, _yM, EdgeLabel(leftM, coefficientM, rightM)) <- xM.outgoing) {
          val yM = base.gen(_yM.value)
          for ((coefficientN, rightN, yN) <- xN.deltaOutputtingLeft(rightM.factors)) {
            val c = inLeft(coefficientM) * inRight(coefficientN)
            result.addStructureMap(((xM <*> xN).forceGen :<*> rightN).forceGen, c *: (leftM.toElement <*>: (yM <*> yN)))
          }
        }
      }
      result
    }
  }

  implicit class TypeADTensorProducts[K1](base: TypeAD[K1]) {
    def <*>[K2](other: TypeAA[K2]): TypeAA[(K1, K2)] = {
      val (inLeft, inRight) = base.rightScalarAction.pushoutInclusions(other.leftScalarAction)
      implicit val result: TypeAA[(K1, K2)] = new TypeAA(inLeft.target, base.leftAlgebra, other.rightAlgebra,
        inLeft.compose(base.leftScalarAction), inRight.compose(other.rightScalarAction))()

      for (gM <- base.gens;
           gN <- other.gens if gM.rightIdempotent == gN.leftIdempotent) {
        result.addGenerator((gM <*> gN).forceGen.label)
      }
      for (xM <- base.gens;
           xN <- other.gens if xM.rightIdempotent == xN.leftIdempotent) {
        for (LkDiEdge(_, _yN, EdgeLabel(leftN, coefficientN, rightN)) <- xN.outgoing) {
          val yN = other.gen(_yN.value)
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
