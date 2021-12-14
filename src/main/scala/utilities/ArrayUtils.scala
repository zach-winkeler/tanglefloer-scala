package utilities

object ArrayUtils {
  implicit class RangeOps[A](as: Array[A]) {
    def update(r: Range, bs: Iterable[A]): Unit =
      r.zip(bs).foreach({ case (i, b) => as(i) = b })
  }
}