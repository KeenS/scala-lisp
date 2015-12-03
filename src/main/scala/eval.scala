package scalalisp



trait Eval[E <: Expr] {
  type Out <: Expr
}

trait EvalLowerPriority{ 
  type List1[E<:Expr] = ConsCell[E, Nil]
  type List2[E1<:Expr, E2<:Expr] = ConsCell[E1, List1[E2]]
  type List3[E1<:Expr, E2<:Expr, E3<:Expr] = ConsCell[E1, List2[E2, E3]]
  type List4[E1<:Expr, E2<:Expr, E3<:Expr, E4 <: Expr] = ConsCell[E1, List3[E2, E3, E4]]
  implicit def ifT[Cond <: Expr, CondOut <:Expr, Then <: Expr, ThenOut <:Expr, Else <: Expr]
    (implicit
      cond : Eval[Cond]{type Out = CondOut},
      then_ : Eval[Then]{type Out = ThenOut})
      : Eval[List4[Symbol[SIf], Cond, Then, Else]] {type Out =  ThenOut} =
    new Eval[List4[Symbol[SIf], Cond, Then, Else]] {type Out =  ThenOut}

}

object Eval extends EvalLowerPriority {
  def apply[E <: Expr](implicit eval: Eval[E]): Eval[E]{type Out = eval.Out} = eval

  implicit def zero: Eval[Zero] {type Out = Zero} = new Eval[Zero] {type Out = Zero}
  implicit def succ[E <: Expr]: Eval[Succ[E]] {type Out = Succ[E]} = new Eval[Succ[E]] {type Out = Succ[E]}
  implicit def nil: Eval[Nil] {type Out = Nil} = new Eval[Nil] {type Out = Nil}
  implicit def t: Eval[T_] {type Out = T_} = new Eval[T_] {type Out = T_}

  //TODO evaluate to bound value
  implicit def symbol[S <: Sym]: Eval[Symbol[S]] {type Out = Symbol[S]} = new Eval[Symbol[S]] {type Out = Symbol[S]}

  implicit def quote[E <: Expr]: Eval[List2[Symbol[SQuote], E]] {type Out = E} =
    new Eval[List2[Symbol[SQuote], E]] {type Out = E}

  implicit def ifNil[Cond <: Expr, Then <: Expr, Else <: Expr, ElseOut <:Expr]
    (implicit
      cond : Eval[Cond]{type Out = Nil},
      else_ : Eval[Else]{type Out = ElseOut}
    ) : Eval[List4[Symbol[SIf], Cond, Then, Else]] {type Out = ElseOut} =
    new Eval[List4[Symbol[SIf], Cond, Then, Else]] {type Out = ElseOut}

  implicit def car[E <: Expr, EOut <: Expr,  CarOut <: Expr]
    (implicit
      e : Eval[E]{type Out = EOut},
      car : Car[EOut]{type Out = CarOut}
    ) : Eval[List2[Symbol[SCar], E]] {type Out = CarOut} =
    new Eval[List2[Symbol[SCar], E]] {type Out = CarOut}

  implicit def cdr[E <: Expr, EOut <: Expr, CdrOut <: Expr]
    (implicit
      e : Eval[E]{type Out = EOut},
      cdr : Cdr[EOut]{type Out = CdrOut}
    ) : Eval[List2[Symbol[SCdr], E]] {type Out = CdrOut} =
    new Eval[List2[Symbol[SCdr], E]] {type Out = CdrOut}

  implicit def cons[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, ConsOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      cons : Cons[L1Out, L2Out]{type Out = ConsOut}
    ) : Eval[List3[Symbol[SCons], L1, L2]] {type Out = ConsOut} =
    new Eval[List3[Symbol[SCons], L1, L2]] {type Out = ConsOut}


  implicit def append[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, AppendOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      append : Append[L1Out, L2Out]{type Out = AppendOut}
    ) : Eval[List3[Symbol[SAppend], L1, L2]] {type Out = AppendOut} =
    new Eval[List3[Symbol[SAppend], L1, L2]] {type Out = AppendOut}


  implicit def plus[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, PlusOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      plus : Plus[L1Out, L2Out]{type Out = PlusOut}
    ) : Eval[List3[Symbol[SPlus], L1, L2]] {type Out = PlusOut} =
    new Eval[List3[Symbol[SPlus], L1, L2]] {type Out = PlusOut}


  implicit def minus[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, MinusOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      minus : Minus[L1Out, L2Out]{type Out = MinusOut}
    ) : Eval[List3[Symbol[SMinus], L1, L2]] {type Out = MinusOut} =
    new Eval[List3[Symbol[SMinus], L1, L2]] {type Out = MinusOut}


  implicit def mult[L1 <: Expr, L2 <: Expr, L1Out <: Expr, L2Out<: Expr, MultOut <: Expr]
    (implicit
      l1 : Eval[L1]{type Out = L1Out},
      l2 : Eval[L2]{type Out = L2Out},
      mult : Mult[L1Out, L2Out]{type Out = MultOut}
    ) : Eval[List3[Symbol[SMult], L1, L2]] {type Out = MultOut} =
    new Eval[List3[Symbol[SMult], L1, L2]] {type Out = MultOut}





  trait EvalToString[E <: Expr] {def apply(): String}

  object EvalToString {
    def apply[E <: Expr](
      implicit evalToString: EvalToString[E]): String = evalToString()
  }
  implicit def evalToString[E <:Expr, EvalOut <: Expr](
    implicit
      eval : Eval[E]{ type Out = EvalOut},
    toString_ : Expr.ToString[EvalOut]
  ): EvalToString[E] = new EvalToString[E]{def apply() = toString_()}
}


