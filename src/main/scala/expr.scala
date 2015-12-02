package scalalisp

sealed trait Expr
sealed trait ConsCell[+H <: Expr, +T <: Expr] extends Expr
sealed trait Nil extends Expr
sealed trait Bool extends Expr
sealed trait True extends Expr
sealed trait False extends Expr
sealed trait Zero extends Expr
sealed trait Succ[N <: Expr] extends Expr

object Expr {
  trait ToString[E <: Expr] {
    def apply(): String
  }
  object ToString {
    def apply[E <: Expr](implicit toString :ToString[E]): String = toString()
  }
  implicit val toStringNil = new ToString[Nil]{
    def apply() = "()"
  }

  implicit val toString0 = new ToString[Zero] {
    def apply() = "0"
  }

  trait ToInt[E <: Expr] {
    def apply(): Int
  }
  object ToInt {
    def apply[E <: Expr](implicit toInt: ToInt[E]): Int = toInt()
  }

  implicit def toInt0:ToInt[Zero] = new ToInt[Zero] {
    def apply() = 0
  }


  implicit def toIntSucc[N <: Expr](implicit toInt: ToInt[N]):ToInt[Succ[N]] = new ToInt[Succ[N]] {
    def apply() = 1 + toInt()
  }
}