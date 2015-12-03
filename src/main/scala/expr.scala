package scalalisp

sealed trait Expr
sealed trait ConsCell[+H <: Expr, +T <: Expr] extends Expr
sealed trait Nil extends Expr
sealed trait T_ extends Expr
sealed trait Bool extends Expr
sealed trait Zero extends Expr
sealed trait Succ[N <: Expr] extends Expr
sealed trait Symbol[S <: Sym] extends Expr

sealed trait Sym
sealed trait SCar extends Sym
sealed trait SCdr extends Sym
sealed trait SCons extends Sym
sealed trait SAppend extends Sym
sealed trait SPlus extends Sym
sealed trait SMinus extends Sym
sealed trait SMult extends Sym
sealed trait SQuote extends Sym
sealed trait SIf extends Sym

object Expr {
  trait ToString[E <: Expr] {
    def apply(): String
  }
  object ToString {
    def apply[E <: Expr](implicit toString :ToString[E]): String = toString()
  }
  implicit def toStringNil = new ToString[Nil]{
    def apply() = "()"
  }
  implicit def toStringT_ = new ToString[T_]{
    def apply() = "t"
  }

  implicit def toStringList[E<:Expr](implicit toStringList : ToStringList[E]): ToString[E] = new ToString[E] {
    def apply() = s"(${ToStringList[E]}"
  }

  trait ToStringList[E <: Expr] {
    def apply(): String
  }
  object ToStringList {
    def apply[E<:Expr](implicit toStringList: ToStringList[E]):String = toStringList()
  }

  implicit def toStringList1[E<:Expr](implicit toString1: ToString[E])
      :ToStringList[ConsCell[E, Nil]] = new ToStringList[ConsCell[E, Nil]] {
    def apply() = s"${toString1()})"
  }

  implicit def toStringListN[E<:Expr, E1 <: Expr, E2 <: Expr ](implicit toString1: ToString[E], toStringList: ToStringList[ConsCell[E1, E2]])
      :ToStringList[ConsCell[E, ConsCell[E1, E2]]] = new ToStringList[ConsCell[E, ConsCell[E1, E2]]] {
    def apply() = s"${toString1()} ${toStringList()}"
  }

  implicit def toStringListCons[E1 <: Expr, E2 <: Expr ](
    implicit toStringCar: ToString[E1],
    toStringCdr: ToString[E2]): ToStringList[ConsCell[E1, E2]] = new ToStringList[ConsCell[E1, E2]]{
    def apply() = s"${toStringCar()} . ${toStringCdr()})"
  }


  // implicit def toString0 = new ToString[Zero] {
  //   def apply() = "0"
  // }

  implicit def toStringSucc[N <:Expr](implicit toInt: ToInt[N]):ToString[N] = new ToString[N] {
    def apply() = ToInt[N].toString
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

  implicit def toStringSym[E <: Expr](implicit toStringSym: ToStringSymbol[E]): ToString[E] = new ToString[E] {
    def apply() = toStringSym()
  }

  trait ToStringSymbol[E <: Expr] {
    def apply(): String
  }

  object ToStringSymbol {
    def apply[E <: Expr](implicit toStringSymbol: ToStringSymbol[E]) = toStringSymbol()
  }

  implicit def toStringCar: ToStringSymbol[Symbol[SCar]] = new ToStringSymbol[Symbol[SCar]] {
    def apply() = "car"
  }

  implicit def toStringCdr: ToStringSymbol[Symbol[SCdr]] = new ToStringSymbol[Symbol[SCdr]] {
    def apply() = "cdr"
  }

  implicit def toStringCons: ToStringSymbol[Symbol[SCons]] = new ToStringSymbol[Symbol[SCons]] {
    def apply() = "cons"
  }

  implicit def toStringAppend: ToStringSymbol[Symbol[SAppend]] = new ToStringSymbol[Symbol[SAppend]] {
    def apply() = "append"
  }

    implicit def toStringPlus: ToStringSymbol[Symbol[SPlus]] = new ToStringSymbol[Symbol[SPlus]] {
    def apply() = "+"
  }

  implicit def toStringMinus: ToStringSymbol[Symbol[SMinus]] = new ToStringSymbol[Symbol[SMinus]] {
    def apply() = "-"
  }

  implicit def toStringMult: ToStringSymbol[Symbol[SMult]] = new ToStringSymbol[Symbol[SMult]] {
    def apply() = "-"
  }

  implicit def toStringQuote: ToStringSymbol[Symbol[SQuote]] = new ToStringSymbol[Symbol[SQuote]] {
    def apply() = "quote"
  }

  implicit def toStringIf: ToStringSymbol[Symbol[SIf]] = new ToStringSymbol[Symbol[SIf]] {
    def apply() = "if"
  }




}
