package algebras

import algebras.Sign.Positive
import algebras.TensorAlgebra.{AMinusElementExtensions, AMinusGeneratorExtensions}
import org.scalatest.funsuite.AnyFunSuite

class TensorAlgebraTests extends AnyFunSuite {
  private val A = new AMinus(IndexedSeq(Set(Positive)))
  private val R = A.ring
  private val TA = new TensorAlgebra(A)
  private val u0 = R("u0")
  private val a = A.gen(Set(0f -> 0f))
  private val b = A.gen(Set(0f -> 1f))
  private val c = A.gen(Set(1f -> 1f))
  private val d = A.gen(Set(1f -> 0f))
  private val da =
    new TensorAlgebra.Element(TA, Map(
      new TensorAlgebra.Generator(TA, IndexedSeq(d, a)) -> R.one
    ))

  test("tensor product") {
    assertResult(TA.zero) {a.toTensorAlgebra(TA) :<*> d}
    assertResult(da) {d.toTensorAlgebra(TA) :<*> a}
    assertResult(u0 *: (a <*>: a.toTensorAlgebra(TA))) {(b * d) <*>: a.toTensorAlgebra(TA).toElement}
  }
}