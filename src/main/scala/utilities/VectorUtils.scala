package utilities

object IndexedSeqUtils {
  implicit class IndexedSeqImprovements[T](v: IndexedSeq[T]) {
    def indicesWhere(p: T => Boolean): IndexedSeq[Int] = v.zipWithIndex.filter(e => p(e._1)).map(_._2)
  }
}