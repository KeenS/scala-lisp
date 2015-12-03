package scalalisp

object Main {
  def main(args: Array[String]) = {
    println(Macro.lisp("1"))
    println(Macro.lisp("(1 (nil) 2 3)"))
    println(Macro.lisp("(+ 1 2)"))
  }
}










