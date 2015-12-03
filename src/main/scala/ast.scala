package scalalisp

object Ast {
  sealed trait Ast
  final case class LNil() extends Ast
  final case class LT() extends Ast
  final case class LCons(car: Ast, cdr: Ast) extends Ast
  final case class LInt(i: Int) extends Ast
  final case class LSymbol(s: String) extends Ast
}
