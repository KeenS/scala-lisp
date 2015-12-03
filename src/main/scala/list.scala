package scalalisp

trait Car[C <: Expr] {type Out <: Expr}

object Car {
  implicit def car[H <: Expr, T <: Expr]: Car[ConsCell[H, T]] {type Out = H} =
    new Car[ConsCell[H, T]] {type Out = H}
}

trait Cdr[C <: Expr] {type Out <: Expr}

object Cdr {
  implicit def cdr[H <: Expr, T <: Expr]: Cdr[ConsCell[H, T]] {type Out = T} =
    new Cdr[ConsCell[H, T]] {type Out = T}
}

trait Cons[Car <: Expr, Cdr <: Expr] {type Out <: Expr}

object Cons {
  implicit def cons[Car <: Expr, Cdr <: Expr]: Cons[Car, Cdr]{type Out = ConsCell[Car, Cdr]} =
    new Cons[Car, Cdr]{type Out = ConsCell[Car, Cdr]}
}


trait Append[L1 <: Expr, L2 <: Expr]{type Out <: Expr}

object Append {
  implicit def appendNil[L1 <: Nil, L2 <: Expr]: Append[L1, L2] {type Out = L2} = new Append[L1, L2] {type Out = L2}

  implicit def appendList[L1Car <: Expr, L1Cdr <: Expr, L2 <: Expr]
    (implicit append_ : Append[L1Cdr, L2]): Append[ConsCell[L1Car, L1Cdr], L2] {type Out = ConsCell[L1Car, append_.Out]} =
    new Append[ConsCell[L1Car, L1Cdr], L2] {type Out = ConsCell[L1Car, append_.Out]}

}
