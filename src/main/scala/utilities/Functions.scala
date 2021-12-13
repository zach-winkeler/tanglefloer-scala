package utilities

object Functions {
  def partialBijections[S,T](source: Set[S], target: Set[T]): Set[Map[S,T]] = {
    var result = Set.empty[Map[S,T]]
    for (subset <- source.subsets()) {
      result ++= injections(subset, target)
    }
    result
  }

  def injections[S,T](source: Set[S], target: Set[T]): Set[Map[S,T]] = {
    if (source.isEmpty) {
      Set(Map.empty)
    } else {
      val x = source.head
      target.flatMap(y => injections(source - x, target - y).map(_ + (x -> y)))
    }
  }

  implicit def tuple2ToIndexedSeq[T](t: (T,T)): IndexedSeq[T] = IndexedSeq(t._1, t._2)
}
