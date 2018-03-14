package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal {
      val bVal = b()
      Math.pow(bVal, 2) - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def compute(op: (Double, Double) => Double) = op(-b(), Math.sqrt(delta())) / (2 * a())
    Signal {
      if (delta() < 0) Set()
      else Set(compute(_ + _), compute(_ - _))
    }
  }
}
