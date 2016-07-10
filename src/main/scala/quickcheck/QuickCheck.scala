package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    elem <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(elem, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (x: Int, y: Int) =>
    val h1 = insert(x, empty)
    val h2 = insert(y, h1)
    if (x < y)
      findMin(h2) == x
    else
      findMin(h2) == y
  }

  property("del1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val afterDeletionHeap = deleteMin(h)
    isEmpty(afterDeletionHeap)
  }

  property("sorted1") = forAll { (h: H) =>
    def getList(h: H) = {
      def rec(h: H, list: List[A]): List[A] = {
        if (isEmpty(h)) list
        else {
          val min = findMin(h)
          rec(deleteMin(h), list ++ List(min))
        }
      }
      rec(h, Nil)
    }
    def isSorted(xs: List[A]): Boolean = xs match {
      case Nil => true
      case h :: t =>
        if (t.isEmpty) true
        else {
          val b = t.head
          if (ord.lteq(h, b)) isSorted(t)
          else false
        }
    }
    isSorted(getList(h))
  }

  property("melding min1") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)

    val h = meld(h1, h2)
    val m = findMin(h)

    if (m1 < m2) m == m1 else m == m2
  }
}
