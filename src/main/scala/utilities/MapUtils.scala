package utilities

object MapUtils {
  implicit class MapImprovements[K1,V](m: Map[K1,V]) {
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
  }
}