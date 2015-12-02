package scalalisp

trait Nat
object Nat {
  type _0 = Zero
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]

}
import Nat._

trait Plus[N <: Expr, M <: Expr] {
  type Out <: Expr
}

trait Minus[N <: Expr, M <: Expr] {
  type Out <: Expr
}

trait Mult[N <: Expr, M <: Expr] {
  type Out <: Expr
}

trait Div[N <: Expr, M <: Expr] {
  type Out <: Expr
}

object Plus {
  implicit def zero[N <: Expr]: Plus[N, Zero] { type Out = N } =
    new Plus[N, Zero] {
      type Out = N
    }
  implicit def succ[N <: Expr, M <: Expr](implicit plus: Plus[N, M]): Plus[N, Succ[M]] { type Out = Succ[plus.Out] } =
    new Plus[N, Succ[M]] {
      type Out = Succ[plus.Out]
    }
}


object Minus {
  implicit def zero[N <: Expr]: Minus[N, Zero] { type Out = N } =
    new Minus[N, Zero] {
      type Out = N
    }
  implicit def succ[N <: Expr, M <: Expr](implicit minus: Minus[N, M]): Minus[Succ[N], Succ[M]] { type Out = minus.Out } =
    new Minus[Succ[N], Succ[M]] {
      type Out = minus.Out
    }
}

object Mult {
  implicit def zero[N <: Expr]: Mult[N, Zero] { type Out = Zero } =
    new Mult[N, Zero] {
      type Out = Zero
    }
  implicit def one[N <: Expr]: Mult[N, Succ[Zero]] { type Out = N } =
    new Mult[N, Succ[Zero]] {
      type Out = N
    }
  implicit def succ[N <: Expr, M <: Expr, R <: Expr](implicit mult: Mult[N, Succ[M]] { type Out = R }, plus: Plus[N, R]): Mult[N, Succ[Succ[M]]] { type Out = plus.Out } =
    new Mult[N, Succ[Succ[M]]] {
      type Out = plus.Out
    }
}



