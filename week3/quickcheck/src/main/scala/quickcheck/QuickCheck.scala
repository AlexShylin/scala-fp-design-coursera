package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec
import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[A]
      h <- oneOf(genHeap, const(empty))
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of empty") = forAll { (_: H) =>
    throws(classOf[NoSuchElementException])(findMin(empty))
  }

  property("insertion") = forAll { (_: H) =>
    val minVal = Int.MinValue
    val maxVal = Int.MaxValue
    val heapWithMin = insert(minVal, empty)
    val fullHeap = insert(maxVal, heapWithMin)
    findMin(fullHeap) == minVal
  }

  property("deleting1") = forAll { (_: H) =>
    val minVal = Int.MinValue
    val maxVal = Int.MaxValue
    val heapWithMin = insert(minVal, empty)
    val fullHeap = insert(maxVal, heapWithMin)
    findMin(deleteMin(fullHeap)) == maxVal
  }

  property("deleting2") = forAll { (_: H) =>
    val minVal = 0
    val heapWithMin = insert(minVal, empty)
    isEmpty(deleteMin(heapWithMin))
  }

  property("sorted seq") = forAll { (h: H) =>

    @tailrec
    def checkSorted(h: H): Boolean = {
      if (isEmpty(h) || isEmpty(deleteMin(h))) true
      else {
        val next = deleteMin(h)
        if (findMin(h) < findMin(next)) checkSorted(next)
        else false
      }
    }

    checkSorted(h)
  }

  property("min of 2 melded") = forAll { (firstHeap: H) =>
    val minVal = new Random().nextInt()
    val heapWithMin = insert(minVal, empty)
    val heapWithMid = insert({
      if (minVal == Int.MaxValue) minVal else minVal + 1
    }, heapWithMin)
    val secondHeap = insert(Int.MaxValue, heapWithMid)
    val meldedHeap = meld(firstHeap, secondHeap)

    val minOfBoth = {
      if (!isEmpty(firstHeap)) Math.min(findMin(firstHeap), findMin(secondHeap))
      else findMin(secondHeap)
    }
    val minOfMelded = findMin(meldedHeap)
    minOfBoth == minOfMelded
  }
}
