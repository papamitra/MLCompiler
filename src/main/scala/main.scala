
package localhost.ml

import scala.util.parsing.combinator._

class MLParser extends JavaTokenParsers{
  def int: Parser[Any] = decimalNumber
  def bool: Parser[Any] = "true" | "false"
  def value: Parser[Any] = int | bool
  def exp: Parser[Any] = value | exp ~prim~exp | "if"~exp~"then"~exp~"else"~exp
  def prim: Parser[Any] = "+" | "-" | "*" | "<"
}

object Main extends App{
  println("Hello,World")
}
