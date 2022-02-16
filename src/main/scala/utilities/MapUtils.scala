package utilities

object MapUtils {
  implicit class MapExtensions[K1,V](val m: Map[K1,V]) extends AnyVal {
    def mapKeysAndMerge[K2](f: K1 => K2, g: (V, V) => V): Map[K2,V] = {
      var result = Map.empty[K2,V]
      for ((k1, v) <- m) {
        val k2 = f(k1)
        if (result.contains(k2)) {
          result = result.updated(k2, g(result(k2), v))
        } else {
          result += k2 -> v
        }
      }
      result
    }

    def mapKeys[K2](f: K1 => K2): Map[K2, V] = m.map({ case (a, b) => (f(a), b) })
  }
}