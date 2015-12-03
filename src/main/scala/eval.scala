package scalalisp


trait Eval[E <: Expr] {
  type Out <: Expr
}

object Eval {
  def apply[E <: Expr](implicit eval: Eval[E]): Aux[E, eval.Out] = eval
  type Aux[E <:Expr, O <: Expr] = Eval[E]{type Out = O}

  implicit def zero: Aux[Zero, Zero] = new Eval[Zero] {
    type Out = Zero
  }

  implicit def succ[E <: Expr]: Aux[Succ[E], Succ[E]] = new Eval[Succ[E]] {
    type Out = Succ[E]
  }

  implicit def nil: Aux[Nil, Nil] = new Eval[Nil] {
    type Out = Nil
  }

  //TODO evaluate to bound value
  implicit def symbol[S <: Sym]: Aux[Symbol[S], Symbol[S]] = new Eval[Symbol[S]] {
    type Out = Symbol[S]
  }

  implicit def car[H <: Expr, T <: Expr, HOut <: Expr, TOut<: Expr, CarOut <: Expr]
    (implicit
      h : Eval[H]{type Out = HOut},
      t : Eval[T]{type Out = TOut},
      car : Car[ConsCell[HOut, TOut]]{type Out = CarOut})
      : Aux[ConsCell[Symbol[SCar], ConsCell[H, T]], CarOut] =
    new Eval[ConsCell[Symbol[SCar], ConsCell[H, T]]] {
    type Out = CarOut
  }

  implicit def cdr[H <: Expr, T <: Expr, HOut <: Expr, TOut<: Expr, CdrOut <: Expr]
    (implicit
      h : Eval[H]{type Out = HOut},
      t : Eval[T]{type Out = TOut},
      cdr : Cdr[ConsCell[HOut, TOut]]{type Out = CdrOut})
      : Aux[ConsCell[Symbol[SCdr], ConsCell[H, T]], CdrOut] =
    new Eval[ConsCell[Symbol[SCdr], ConsCell[H, T]]] {
    type Out = CdrOut
  }

  implicit def append[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, AppendOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      append : Append[L1Out, L2Out]{type Out = AppendOut})
      : Aux[ConsCell[Symbol[SAppend], ConsCell[L1, ConsCell[L2, Nil]]], AppendOut] =
    new Eval[ConsCell[Symbol[SAppend], ConsCell[L1, ConsCell[L2, Nil]]]] {
    type Out = AppendOut
  }



}
