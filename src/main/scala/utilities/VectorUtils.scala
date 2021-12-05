package utilities

object VectorUtils {
  implicit class VectorImprovements[T](v: Vector[T]) {
    def indicesWhere(p: T => Boolean): Vector[Int] = v.zipWithIndex.filter(e => p(e._1)).map(_._2)
  }
}