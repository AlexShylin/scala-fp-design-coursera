package exercise

object ex extends App {

  object repeat {
    sealed abstract class RepeatCons() {
      def until(condition: => Boolean): Unit
    }

    def apply(command: => Unit): RepeatCons = new RepeatCons() {
      command

      def until(condition: => Boolean): Unit = {
        if (condition) repeat(command).until(condition)
        else ()
      }
    }
  }

  var r = 5
  repeat {
    println(r)
    r -= 1
  } until (r > 0)

}
