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

  implicit def quote[E <: Expr]: Aux[ConsCell[Symbol[SQuote], ConsCell[E, Nil]], E] =
    new Eval[ConsCell[Symbol[SQuote], ConsCell[E, Nil]]] {
      type Out = E
    }

  implicit def car[E <: Expr, EOut <: Expr,  CarOut <: Expr]
    (implicit
      e : Eval[E]{type Out = EOut},
      car : Car[EOut]{type Out = CarOut})
      : Aux[ConsCell[Symbol[SCar], ConsCell[E, Nil]], CarOut] =
    new Eval[ConsCell[Symbol[SCar], ConsCell[E, Nil]]] {
    type Out = CarOut
  }

  implicit def cdr[E <: Expr, EOut <: Expr, CdrOut <: Expr]
    (implicit
      e : Eval[E]{type Out = EOut},
      cdr : Cdr[EOut]{type Out = CdrOut})
      : Aux[ConsCell[Symbol[SCdr], ConsCell[E, Nil]], CdrOut] =
    new Eval[ConsCell[Symbol[SCdr], ConsCell[E, Nil]]] {
    type Out = CdrOut
  }

  implicit def cons[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, ConsOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      cons : Cons[L1Out, L2Out]{type Out = ConsOut})
      : Aux[ConsCell[Symbol[SCons], ConsCell[L1, ConsCell[L2, Nil]]], ConsOut] =
    new Eval[ConsCell[Symbol[SCons], ConsCell[L1, ConsCell[L2, Nil]]]] {
    type Out = ConsOut
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


  implicit def plus[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, PlusOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      plus : Plus[L1Out, L2Out]{type Out = PlusOut})
      : Aux[ConsCell[Symbol[SPlus], ConsCell[L1, ConsCell[L2, Nil]]], PlusOut] =
    new Eval[ConsCell[Symbol[SPlus], ConsCell[L1, ConsCell[L2, Nil]]]] {
    type Out = PlusOut
  }


  implicit def minus[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, MinusOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      minus : Minus[L1Out, L2Out]{type Out = MinusOut})
      : Aux[ConsCell[Symbol[SMinus], ConsCell[L1, ConsCell[L2, Nil]]], MinusOut] =
    new Eval[ConsCell[Symbol[SMinus], ConsCell[L1, ConsCell[L2, Nil]]]] {
    type Out = MinusOut
  }


  implicit def mult[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, MultOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      mult : Mult[L1Out, L2Out]{type Out = MultOut})
      : Aux[ConsCell[Symbol[SMult], ConsCell[L1, ConsCell[L2, Nil]]], MultOut] =
    new Eval[ConsCell[Symbol[SMult], ConsCell[L1, ConsCell[L2, Nil]]]] {
    type Out = MultOut
  }





  trait EvalToString[E <: Expr] {
    def apply(): String
  }

  object EvalToString {
    def apply[E <: Expr](
      implicit evalToString: EvalToString[E]): String = evalToString()
  }
  implicit def evalToString[E <:Expr, EvalOut <: Expr](
    implicit eval : Eval[E]{ type Out = EvalOut},
    toString_ : Expr.ToString[EvalOut]): EvalToString[E] = new EvalToString[E]{
    def apply() = toString_()
  }
}


