package scalalisp

trait Car[C <: Expr] {type Out <: Expr}

object Car {
  def apply[C <: Expr](implicit car: Car[C]): Car[C] {type Out = car.Out} = car

  implicit def car[H <: Expr, T <: Expr]: Car[ConsCell[H, T]] {type Out = H} =
    new Car[ConsCell[H, T]] {type Out = H}
}

trait Cdr[C <: Expr] {type Out <: Expr}

object Cdr {
  def apply[C <: Expr](implicit cdr: Cdr[C]): Cdr[C] {type Out = cdr.Out} = cdr

  implicit def cdr[H <: Expr, T <: Expr]: Cdr[ConsCell[H, T]] {type Out = T} =
    new Cdr[ConsCell[H, T]] {type Out = T}
}

trait Cons[Car <: Expr, Cdr <: Expr] {type Out <: Expr}

object Cons {
  def apply[Car <: Expr, Cdr <: Expr](implicit cons : Cons[Car, Cdr]): Cons[Car, Cdr]{type Out = cons.Out}  = cons

  implicit def cons[Car <: Expr, Cdr <: Expr]: Cons[Car, Cdr]{type Out = ConsCell[Car, Cdr]} =
    new Cons[Car, Cdr]{type Out = ConsCell[Car, Cdr]}
}


trait Append[L1 <: Expr, L2 <: Expr]{type Out <: Expr}

object Append {
  def apply[L1 <: Expr, L2 <: Expr](implicit append: Append[L1, L2]):Append[L1, L2] {type Out = append.Out}  = append


  implicit def appendNil[L1 <: Nil, L2 <: Expr]: Append[L1, L2] {type Out = L2} = new Append[L1, L2] {type Out = L2}

  implicit def appendList[L1Car <: Expr, L1Cdr <: Expr, L2 <: Expr]
    (implicit append_ : Append[L1Cdr, L2]): Append[ConsCell[L1Car, L1Cdr], L2] {type Out = ConsCell[L1Car, append_.Out]} =
    new Append[ConsCell[L1Car, L1Cdr], L2] {type Out = ConsCell[L1Car, append_.Out]}

}
