package scalalisp

import util.parsing.combinator._

object Parser extends JavaTokenParsers {

  lazy val expr  =  lnil | lt | llist | lint | lsymbol | lquote 
  lazy val llist:Parser[Ast.Ast] = "(" ~> rep(expr) <~ ")" ^^ {
    l => l.foldRight(Ast.LNil():Ast.Ast) ((e, acc)=> Ast.LCons(e, acc))
  }
  lazy val lnil = "nil" ^^ {_ => Ast.LNil()}
  lazy val lt = "t" ^^ {_ => Ast.LT()}
  lazy val lint = """\d+""".r ^^ {
    n => Ast.LInt(n.toInt)}
  lazy val lsymbol = """[^\s()'0-9]+""".r ^^ {
    s => Ast.LSymbol(s.mkString)
  }

  lazy val lquote: Parser[Ast.Ast] = "'" ~> expr ^^ {
    e => Ast.LCons(Ast.LSymbol("quote"), Ast.LCons(e, Ast.LNil()))
  }
}

