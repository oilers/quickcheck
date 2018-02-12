package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for{
      v <- arbitrary[Int]
      h <- genHeap
    } yield insert(v, h)
  )

  def toList(heap: H): List[Int] = {
    if(isEmpty(heap)) List()
    else findMin(heap)::toList(deleteMin(heap))
  }


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insertTwo") = forAll{(x1: Int, x2: Int) =>
    val heap = insert(x2, insert(x1, empty))

    findMin(heap) == min(x1, x2)
  }

  property("emptyHeap") = forAll{(x: Int) =>
    val heap = insert(x, empty)
    isEmpty(deleteMin(heap))
  }

  property("sortedHeap") = forAll{(h: H) =>

    def isSorted(xs: List[Int], heap: H): Boolean = xs match {
      case min::rest => min == findMin(heap) && isSorted(rest, deleteMin(heap))
      case Nil => isEmpty(heap)
    }
    val heapList = toList(h)
    isSorted(heapList, h) && heapList.sorted == heapList
  }

  property("meldedMin") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    if(isEmpty(h1) && isEmpty(h2)) isEmpty(melded)
    else if(isEmpty(h1)) findMin(h2) == findMin(melded)
    else if(isEmpty(h2)) findMin(h1) == findMin(melded)
    else min(findMin(h1), findMin(h2)) == findMin(melded)
  }

  property("meldAssociativity") = forAll{(h1: H, h2: H) =>
    val melded = meld(h1, h2)
    def compare(right: H, left: H): Boolean = {
      if(isEmpty(left) && isEmpty(right)) true
      else if(isEmpty(left) || isEmpty(right)) false
      else findMin(right) == findMin(left) && compare(deleteMin(right), deleteMin(left))
    }
    if(isEmpty(h1) && isEmpty(h2)) isEmpty(melded)
    else if (isEmpty(h1)) compare(melded, meld(deleteMin(h2), insert(findMin(h2), h1)))
    else compare(melded, meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
