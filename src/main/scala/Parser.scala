
package localhost.ml

import scala.util.parsing.combinator._

abstract sealed class Op
object Add extends Op
object Sub extends Op
object Mul extends Op
object Less extends Op

abstract sealed class Expr
case class IntLiteral(val value:Int) extends Expr
case class IfExpr(val pred:Expr, val texpr:Expr, val fexpr:Expr) extends Expr
case class BinOp(val op:Op, val lhs:Expr , val rhs:Expr) extends Expr
case class BoolLiteral(val b:Boolean) extends Expr
case class Symbol(val symName:String) extends Expr
case class LetExpr(val sym:Symbol, val d:Expr, val e:Expr) extends Expr

class MLParser extends JavaTokenParsers{
  val reserved = List("true", "false", "if", "then", "else", "let", "in")
  def int: Parser[Expr] = (decimalNumber ^^ { case n=> IntLiteral(n.toInt)} | "-"~decimalNumber ^^ { case "-"~n => IntLiteral(-n.toInt)} )
  def bool: Parser[Expr] = "true" ^^ (_=>BoolLiteral(true)) | "false" ^^ (_=>BoolLiteral(false))
//  def value: Parser[Expr] = int | bool
  def symbol : Parser[Symbol] = ident ^^ (Symbol(_))
  def factor: Parser[Expr] = int | bool | "if"~expr~"then"~expr~"else"~expr ^^ { case "if"~pred~"then"~texpr~"else"~fexpr => IfExpr(pred, texpr, fexpr)} | "let"~symbol~"="~expr~"in"~expr ^^ {case "let"~sym~"="~d~"in"~e => LetExpr(sym,d,e)} | symbol | "("~>expr<~")"
  def term: Parser[Expr] = factor~"*"~term ^^ { case lhs~"*"~rhs => BinOp(Mul, lhs, rhs)} | factor
  def exp: Parser[Expr] = term~"+"~exp ^^ {case lhs~"+"~rhs => BinOp(Add,lhs,rhs)} | term~"-"~exp ^^ {case lhs~"-"~rhs => BinOp(Sub,lhs,rhs)} | term
  def expr: Parser[Expr] = exp~"<"~exp ^^ {case lhs~"<"~rhs => BinOp(Less,lhs,rhs)} | exp
}
