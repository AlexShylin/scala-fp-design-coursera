package quickcheck

trait IntHeap extends Heap {
  override type A = Int
  override def ord = scala.math.Ordering.Int
}

// http://www.brics.dk/RS/96/37/BRICS-RS-96-37.pdf

// Figure 1, page 3
trait Heap {
  /**
    * type of a heap
    */
  type H

  /**
    * type of an element
    */
  type A

  /**
    * @return ordering on elements
    */
  def ord: Ordering[A]

  /**
    * @return the empty heap
    */
  def empty: H

  /**
    * Whether the given heap <code>h</code> is empty
    * @param h given heap
    * @return true if is empty, false otherwise
    */
  def isEmpty(h: H): Boolean

  /**
    * The heap resulting from inserting <code>x</code> into <code>h</code>
    */
  def insert(x: A, h: H): H

  /**
    * The heap resulting from merging <code>h1</code> and <code>h2</code>
    */
  def meld(h1: H, h2: H): H

  /**
    * A minimum of the heap <code>h</code>
    */
  def findMin(h: H): A

  /**
    * A heap resulting from deleting a minimum of <code>h</code>
    */
  def deleteMin(h: H): H
}

// Figure 3, page 7
trait BinomialHeap extends Heap {

  type Rank = Int
  case class Node(x: A, r: Rank, c: List[Node])
  override type H = List[Node]

  protected def root(t: Node): A = t.x

  protected def rank(t: Node): Rank = t.r

  protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r
    if (ord.lteq(t1.x, t2.x)) Node(t1.x, t1.r+1, t2 :: t1.c)
    else Node(t2.x, t2.r + 1, t1 :: t2.c)

  protected def ins(t: Node, ts: H): H = ts match {
    case Nil => List(t)
    case tp :: tail => // t.r<=tp.r
      if (t.r < tp.r) t :: tp :: tail else ins(link(t, tp), tail)
  }

  override def empty = Nil

  override def isEmpty(ts: H): Boolean = ts.isEmpty

  override def insert(x: A, ts: H): H =
    ins(Node(x, 0 , Nil), ts)

  override def meld(ts1: H, ts2: H): H = (ts1, ts2) match {
    case (Nil, ts) => ts
    case (ts, Nil) => ts
    case (t1 :: tail1, t2 :: tail2) =>
      if (t1.r < t2.r) t1 :: meld(tail1, t2 :: tail2)
      else if (t2.r < t1.r) t2 :: meld(t1 :: tail1, tail2)
      else ins(link(t1, t2), meld(tail1, tail2))
  }

  override def findMin(ts: H): A = ts match {
    case Nil => throw new NoSuchElementException("min of empty heap")
    case t :: Nil => root(t)
    case t :: tail =>
      val x = findMin(tail)
      if (ord.lteq(root(t),x)) root(t) else x
  }

  override def deleteMin(ts: H): List[Node] = ts match {
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case t :: tail =>
      def getMin(t: Node, ts: H): (Node, H) = ts match {
        case Nil => (t, Nil)
        case tp :: tsp =>
          val (tq, tsq) = getMin(tp, tsp)
          if (ord.lteq(root(t), root(tq))) (t, ts) else (tq, t :: tsq)
      }
      val (Node(_, _, c), tsq) = getMin(t, tail)
      meld(c.reverse, tsq)
  }
}

trait Bogus1BinomialHeap extends BinomialHeap {
  override def findMin(ts: H): A = ts match {
    case Nil => throw new NoSuchElementException("min of empty heap")
    case t :: _ => root(t)
  }
}

trait Bogus2BinomialHeap extends BinomialHeap {
  override protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r
    if (!ord.lteq(t1.x, t2.x)) Node(t1.x, t1.r + 1, t2 :: t1.c) else Node(t2.x, t2.r + 1, t1 :: t2.c)
}

trait Bogus3BinomialHeap extends BinomialHeap {
  override protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r
    if (ord.lteq(t1.x, t2.x)) Node(t1.x, t1.r + 1, t1 :: t1.c) else Node(t2.x, t2.r + 1, t2 :: t2.c)
}

trait Bogus4BinomialHeap extends BinomialHeap {
  override def deleteMin(ts: H): H = ts match {
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case t :: tail => meld(t.c.reverse, tail)
  }
}

trait Bogus5BinomialHeap extends BinomialHeap {
  override def meld(ts1: H, ts2: H): H = ts1 match {
    case Nil => ts2
    case t1 :: tail1 => List(Node(t1.x, t1.r, tail1 ++ ts2))
  }
}
