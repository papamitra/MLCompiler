
import localhost.ml._
import org.scalatest.FunSuite

class MLParserSuite extends FunSuite{
  object TestParser extends MLParser{
    def parse(args:String)={
      parseAll(expr, args)
    }
  }

  import TestParser._
  test("parse expr"){
    assert(BinOp(Minus, IntLiteral(2), IntLiteral(1)) == parse("2-1").get)
    assert(BinOp(Multi, IntLiteral(1), BinOp(Multi, IntLiteral(2), IntLiteral(3))) == parse("1 * 2 * 3").get)
    assert(BinOp(Plus, IntLiteral(1), BinOp(Plus, IntLiteral(2), IntLiteral(3))) == parse("1 + 2 + 3").get)
    assert(BinOp(Plus, IntLiteral(1), BinOp(Multi, IntLiteral(2), IntLiteral(3))) == parse("1 + 2 * 3").get)
    assert(IfExpr(BinOp(Less, IntLiteral(3), IntLiteral(4)), BoolLiteral(true), IntLiteral(5)) == parse("if 3 < 4 then true else 5").get)
    assert(BinOp(Plus, IntLiteral(5), IfExpr(BinOp(Less, IntLiteral(-3), IntLiteral(-12)), IntLiteral(4), BoolLiteral(false))) == parse("5 + if -3 < -12 then 4 else false").get)
  }

  test("eval expr"){
    var i:Int = 0
    def eval(src:String)={
      GenJVM.vd.clear()
      val code = new GenJVM("Klass" + i,src)
      i += 1
      code.eval
    }
    assert(6 == eval("1+2+3"))
    assert(7 == eval("1+2*3"))
  }
}
