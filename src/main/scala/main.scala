
package localhost.ml

import scala.util.parsing.combinator._

class MLParser extends JavaTokenParsers{
  def int: Parser[Any] = decimalNumber | "-"~decimalNumber
  def bool: Parser[Any] = "true" | "false"
  def value: Parser[Any] = int | bool
  def factor: Parser[Any] = value | "if"~expr~"then"~expr~"else"~expr
  def term: Parser[Any] = factor~"*"~factor | factor
  def exp: Parser[Any] = term~("+"~term | "-"~term)|term
  def expr: Parser[Any] = exp~"<"~exp|exp
}

object Main extends App{
  println("Hello,World")
}
