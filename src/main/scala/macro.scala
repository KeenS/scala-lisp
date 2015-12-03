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
    q"Eval.EvalToString[$t]"
  }

  def toT(c: Context)(e: Ast.Ast): c.Tree = {
    import c.universe._
    import Ast._
    e match {
      case LNil() => tq"Nil"
      case LCons(car, cdr) => tq"ConsCell[${toT(c)(car)},${toT(c)(cdr)}]"
      case LInt(0) => tq"Zero"
      case LInt(i) => tq"Succ[${toT(c)(LInt(i-1))}]"
      case LSymbol("car") => tq"Symbol[SCar]"
      case LSymbol("cdr") => tq"Symbol[SCdr]"
      case LSymbol("cons") => tq"Symbol[SCons]"
      case LSymbol("append") => tq"Symbol[SAppend]"
      case LSymbol("+") => tq"Symbol[SPlus]"
      case LSymbol("-") => tq"Symbol[SMinus]"
      case LSymbol("*") => tq"Symbol[SMult]"
      case LSymbol("quote") => tq"Symbol[SQuote]"
      case LSymbol("if") => tq"Symbol[SIf]"
      //TODO handle error
      case LSymbol(_) => tq"Nil"
    }
  }

}

