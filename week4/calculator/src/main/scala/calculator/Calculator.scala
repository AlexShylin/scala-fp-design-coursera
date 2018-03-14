package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    (for {
      key <- namedExpressions.keys
      value <- namedExpressions.get(key)
    } yield {
      try {
        key -> Signal(eval(value(), namedExpressions))
      } catch {
        case _: IllegalArgumentException => key -> Signal(Double.NaN)
      }
    }).toMap
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references.filter { case (key, _) => key != name }), references)
      case Plus(a, b) => {
        val res = eval(a, references) + eval(b, references)
        println(s"$a + $b = $res")
        res
      }
      case Minus(a, b) => {
        val res = eval(a, references) - eval(b, references)
        println(s"$a - $b = $res")
        res
      }
      case Times(a, b) => {
        val res = eval(a, references) * eval(b, references)
        println(s"$a * $b = $res")
        res
      }
      case Divide(a, b) => {
        val res = eval(a, references) / eval(b, references)
        println(s"$a / $b = $res")
        res
      }
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } {
      exprSignal =>
        exprSignal()
    }
  }

  def main(args: Array[String]): Unit = {
//    computeValues(Map("a" -> Signal(Ref("b")), "b" -> Signal(Ref("a"))))
    println(355d/113d)
  }
}
