
package localhost.ml

import scala.util.parsing.combinator._
import ch.epfl.lamp.fjbg._

abstract sealed class Op{ def emit(code:JExtendedCode)}
object Plus extends Op{  def emit(code:JExtendedCode){ code.emitIADD()}}
object Minus extends Op{  def emit(code:JExtendedCode){ code.emitISUB()}}
object Multi extends Op{  def emit(code:JExtendedCode){ code.emitIMUL()}}
object Less extends Op{
  def emit(code:JExtendedCode){ 
    import code._
    val succ = newLabel()
    val ret = newLabel()
    emitIF_ICMPLT(succ)
    emitLDC(0)
    emitGOTO(ret)
    succ.anchorToNext()
    emitLDC(1)
    ret.anchorToNext()
  }
}

abstract sealed class Expr{
  def emit(code:JExtendedCode):Unit
}

case class IntLiteral(val value:Int) extends Expr{
  def emit(code:JExtendedCode){
    code.emitLDC(value)
  }
}
case class StringLiteral(val value:String) extends Expr{
  def emit(code:JExtendedCode){
    throw new Exception("StringLiteral emit not Implemented")
  }
}
case class IfExpr(val pred:Expr, val texpr:Expr, val fexpr:Expr) extends Expr{
  def emit(code:JExtendedCode){
    import code._
    val fail = newLabel()
    val ret = newLabel()
    pred.emit(code)
    emitIFEQ(fail)
    texpr.emit(code)
    emitGOTO(ret)
    fail.anchorToNext()
    fexpr.emit(code)
    ret.anchorToNext()
  }
}
case class BinOp(val op:Op, val lhs:Expr , val rhs:Expr) extends Expr{
  def emit(code:JExtendedCode){
    lhs.emit(code)
    rhs.emit(code)
    op.emit(code)
  }
}

case class BoolLiteral(val b:Boolean) extends Expr{
  def emit(code:JExtendedCode){
    throw new Exception("Boolean emit not Implemented")
  }
}

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
