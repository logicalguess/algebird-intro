package logicalguess.algebird.simple

import com.twitter.algebird.Monoid

import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.collection.immutable.Queue

class MergeSortMonoid[T](implicit ord: Ordering[T]) extends Monoid[List[T]] {
  def build(value: T) = List(value)

  override def zero: List[T] = List()

  override def plus(ls: List[T], rs: List[T]): List[T] = (ls, rs)  match {
      case (Nil, rest) => rest
      case (rest, Nil) => rest
      case (l :: lRest, r :: rRest) => if (ord.lt(l, r)) l :: plus(lRest, rs) else r :: plus(ls, rRest)
  }
}

object MergeSortMonoid {
  implicit def mergeSortMonoid[T](implicit ord: Ordering[T]) = new MergeSortMonoid[T]

  implicit class PimpSeq[T](val seq: GenSeq[T]) {
    def mergeSort(implicit monoid: MergeSortMonoid[T]): Seq[T] =
      seq.aggregate(monoid.zero)({ case (q, value) => monoid.plus(q, monoid.build(value)) }, monoid.plus)
  }
}

object Test {
  import MergeSortMonoid._
  def main(args: Array[String]): Unit = {
    println(List(1, 7, 3, 15, 2).mergeSort)
  }
}