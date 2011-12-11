
package localhost.ml

import scala.util.parsing.combinator._

abstract sealed class Expr
case class IfExpr(val pred:Expr, texpr:Expr, fexpr:Expr) extends Expr
case class BinOP(val lhs:Expr , val rhs:Expr) extends Expr
case class IntExpr(val i:Int) extends Expr
case class BoolExpr(val b:Boolean) extends Expr

class MLParser extends JavaTokenParsers{
  def int: Parser[Any] = decimalNumber | "-"~decimalNumber
  def bool: Parser[Any] = "true" | "false"
  def value: Parser[Any] = int | bool
  def factor: Parser[Any] = value | "if"~expr~"then"~expr~"else"~expr
  def term: Parser[Any] = factor~"*"~factor | factor
  def exp: Parser[Any] = term~("+"~term | "-"~term)|term
  def expr: Parser[Any] = exp~"<"~exp|exp
}
