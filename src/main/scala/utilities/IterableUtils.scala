package utilities

import scala.annotation.tailrec

object IterableUtils {
  implicit class IterableExtensions[T](val it: Iterable[T]) extends AnyVal {
    def foldUntil[A](zero: A)(until: A => Boolean)(op: (A, T) => A): A = {
      @tailrec def loop(acc: A, remaining: Iterable[T]): A = remaining match {
        case _ if remaining.isEmpty | until(acc) => acc
        case _ => loop(op(acc, remaining.head), remaining.tail)
      }

      loop(zero, it)
    }
  }
}
