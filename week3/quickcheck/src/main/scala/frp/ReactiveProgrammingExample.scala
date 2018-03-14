package frp

import java.util.Optional

import scala.concurrent.Future

object ReactiveProgrammingExample extends App {

  class StackableVariable[T](init: T) {
    private var values: List[T] = List(init)

    def value: T = values.head

    def withValue[R](newValue: T)(op: => R): R = {
      values = newValue :: values
      try op finally values = values.tail
    }
  }

  object Signal {
    private val caller = new StackableVariable[Signal[_]](NoSignal)

    def apply[T](expr: => T) = new Signal(expr)
  }

  class Signal[T](expr: => T) {

    import Signal._

    private var myExpr: () => T = _
    private var myValue: T = _
    private var observers: Set[Signal[_]] = Set()
    update(expr)

    def apply(): T = {
      observers += caller.value
      assert(!caller.value.observers.contains(this), "cyclic signal error")
      myValue
    }

    protected def update(expr: => T): Unit = {
      myExpr = () => expr
      computeValue()
    }

    protected def computeValue(): Unit = {
      val newValue = caller.withValue(this)(myExpr())
      if (newValue != myValue) {
        myValue = newValue
        val obs = observers
        observers = Set()
        obs.foreach(_.computeValue())
      }
    }
  }

  object NoSignal extends Signal[Nothing](???) {
    override def computeValue(): Unit = ()
  }

  class Var[T](expr: => T) extends Signal[T](expr) {
    override def update(expr: => T): Unit = super.update(expr)
  }

  object Var {
    def apply[T](expr: => T) = new Var(expr)
  }

  val a = Var(5)
  val b = Signal(a() * 2)
  a() = 4
  println(b())

  println(List(1,2,3).foldLeft(Optional.of(0))((a: Optional[Int], b) =>  Optional.of(a.get() + b)))

}