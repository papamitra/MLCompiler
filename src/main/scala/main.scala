
package localhost.ml

import scala.util.parsing.combinator._
import ch.epfl.lamp.fjbg._

abstract sealed class Op
object Plus extends Op
object Minus extends Op
object Multi extends Op
object Less extends Op

abstract sealed class Expr
case class IntLiteral(val value:Int) extends Expr
case class StringLiteral(val value:String) extends Expr
case class IfExpr(val pred:Expr, val texpr:Expr, val fexpr:Expr) extends Expr
case class BinOp(val op:Op, val lhs:Expr , val rhs:Expr) extends Expr
case class BoolLiteral(val b:Boolean) extends Expr

class MLParser extends JavaTokenParsers{
  def int: Parser[Expr] = (decimalNumber ^^ { case n=> IntLiteral(n.toInt)}
			   | "-"~decimalNumber ^^ { case "-"~n => IntLiteral(-n.toInt)} )

  def bool: Parser[Expr] = "true" ^^ (_=>BoolLiteral(true)) | "false" ^^ (_=>BoolLiteral(false))
  def value: Parser[Expr] = int | bool
  def factor: Parser[Expr] = value | "if"~expr~"then"~expr~"else"~expr ^^ { case "if"~pred~"then"~texpr~"else"~fexpr => IfExpr(pred, texpr, fexpr)} | expr
  def term: Parser[Expr] = factor~"*"~term ^^ { case lhs~"*"~rhs => BinOp(Multi, lhs, rhs)} | factor
  def exp: Parser[Expr] = term~"+"~exp ^^ {case lhs~"+"~rhs => BinOp(Plus,lhs,rhs)} | term~"-"~exp ^^ {case lhs~"-"~rhs => BinOp(Minus,lhs,rhs)} |term
  def expr: Parser[Expr] = exp~"<"~exp ^^ {case lhs~"<"~rhs => BinOp(Less,lhs,rhs)} | exp
}
