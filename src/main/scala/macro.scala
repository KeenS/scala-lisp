package scalalisp

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object Macro {
  def lisp(expr: String): String = macro lisp_impl
  def lisp_impl(c: Context)(expr: c.Expr[String]): c.Tree = {
    import c.universe._
    val Literal(Constant(s_expr: String)) = expr.tree
    val ast = Parser.parseAll(Parser.expr, s_expr).get
    val t = toT(c)(ast)
    q"Expr.ToString[$t]"
  }

  def toT(c: Context)(e: Ast.Ast): c.Tree = {
    import c.universe._
    import Ast._
    e match {
      case LNil() => tq"Nil"
      case LCons(car, cdr) => tq"ConsCell[${toT(c)(car)},${toT(c)(cdr)}]"
      case LInt(0) => tq"Zero"
      case LInt(i) => tq"Succ[${toT(c)(LInt(i-1))}]"
      case LSymbol(_) => tq"Zero"
    }
  }

}

