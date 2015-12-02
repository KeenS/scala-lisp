package scalalisp

trait Car[C <: Expr] {
  type Out <: Expr
}

object Car {
  def apply[C <: Expr](implicit car: Car[C]): Aux[C, car.Out] = car
  type Aux[C <: Expr, O <: Expr] = Car[C]{type Out = O}
  implicit def car[H <: Expr, T <: Expr]: Aux[ConsCell[H, T], H] = new Car[ConsCell[H, T]] {type Out = H}
}

trait Cdr[C <: Expr] {
  type Out <: Expr
}

object Cdr {
  def apply[C <: Expr](implicit cdr: Cdr[C]): Aux[C, cdr.Out] = cdr
  type Aux[C <: Expr, O <: Expr] = Cdr[C]{type Out = O}
  implicit def cdr[H <: Expr, T <: Expr]: Aux[ConsCell[H, T], T] = new Cdr[ConsCell[H, T]] {type Out = T}
}

trait Cons[Car <: Expr, Cdr <: Expr] {
  type Out <: Expr
}

object Cons {
  def apply[Car <: Expr, Cdr <: Expr](implicit cons : Cons[Car, Cdr]): Aux[Car, Cdr, cons.Out] = cons
  type Aux[Car<: Expr, Cdr <: Expr, O] = Cons[Car, Cdr]{type Out = O}
  implicit def cons[Car <: Expr, Cdr <: Expr]: Aux[Car, Cdr, ConsCell[Car,Cdr]] = new Cons[Car, Cdr]{type Out = ConsCell[Car, Cdr]}
}


trait Append[L1 <: Expr, L2 <: Expr]{type Out <: Expr}

object Append {
  type Aux[L1 <: Expr, L2 <: Expr, O <: Expr] = Append[L1, L2]{type Out = O}
  def apply[L1 <: Expr, L2 <: Expr](implicit append: Append[L1, L2]): Aux[L1, L2, append.Out] = append


  implicit def appendNil[L1 <: Nil, L2 <: Expr]: Aux[L1, L2, L2] = new Append[L1, L2] {
    type Out = L2
  }

  implicit def appendList[L1Car <: Expr, L1Cdr <: Expr, L2 <: Expr]
    (implicit append_ : Append[L1Cdr, L2]): Aux[ConsCell[L1Car, L1Cdr], L2, ConsCell[L1Car, append_.Out]] =
    new Append[ConsCell[L1Car, L1Cdr], L2] {
      type Out = ConsCell[L1Car, append_.Out]
    }

}
