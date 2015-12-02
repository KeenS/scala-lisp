package scalalisp

object Ast {
  sealed trait Ast
  final case class LNil() extends Ast
  final case class LCons(car: Ast, cdr: Ast) extends Ast
  final case class LInt(i: Int) extends Ast
  final case class LSymbol(s: String) extends Ast
  // def lcar(e: Ast): Ast = e match {
  //   case LCons(car, cdr) => car
  //   case  _ => LNil()
  // }

}
