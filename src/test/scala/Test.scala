
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
    assert(BinOp(Sub, IntLiteral(2), IntLiteral(1)) == parse("2-1").get)
    assert(BinOp(Mul, IntLiteral(1), BinOp(Mul, IntLiteral(2), IntLiteral(3))) == parse("1 * 2 * 3").get)
    assert(BinOp(Add, IntLiteral(1), BinOp(Add, IntLiteral(2), IntLiteral(3))) == parse("1 + 2 + 3").get)
    assert(BinOp(Add, IntLiteral(1), BinOp(Mul, IntLiteral(2), IntLiteral(3))) == parse("1 + 2 * 3").get)
    assert(IfExpr(BinOp(Less, IntLiteral(3), IntLiteral(4)), BoolLiteral(true), IntLiteral(5)) == parse("if 3 < 4 then true else 5").get)
    assert(BinOp(Add, IntLiteral(5), IfExpr(BinOp(Less, IntLiteral(-3), IntLiteral(-12)), IntLiteral(4), BoolLiteral(false))) == parse("5 + if -3 < -12 then 4 else false").get)

    assert(BinOp(Add, IntLiteral(1), IntLiteral(2)) == parse("(1) + (2)").get)
    assert(BinOp(Mul, BinOp(Add, IntLiteral(1), IntLiteral(2)) , IntLiteral(3)) == parse("(1+2) * 3").get)
    assert(BinOp(Add, IfExpr(BoolLiteral(true), IntLiteral(1), IntLiteral(2)), IntLiteral(3)) == parse("(if true then 1 else 2) + 3").get)

    assert(BinOp(Add, Symbol("a"), IntLiteral(1)) == parse("a + 1").get)
    assert(LetExpr(Symbol("a"), IntLiteral(1), BinOp(Add, Symbol("a"), IntLiteral(1))) == parse("let a = 1 in a + 1").get)
    assert(BinOp(Sub, IntLiteral(1), BinOp(Add,
      LetExpr(Symbol("a"), BinOp(Add, IntLiteral(1), IntLiteral(2)), BinOp(Mul, Symbol("a"), IntLiteral(2))),
      IntLiteral(3))) == parse("1 - (let a = 1+2 in a * 2) + 3").get)
    assert(LetExpr(Symbol("a"), IntLiteral(2),
		   LetExpr(Symbol("b"), IntLiteral(3),
			   LetExpr(Symbol("a"), IntLiteral(1), BinOp(Add, Symbol("a"), Symbol("b")))))
	   == parse("let a=2 in let b=3 in let a=1 in a+b").get)
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
    assert(1 == eval("1 + if 2+2 < 3 then 3*2 else 0*3"))
//    assert(1 == eval("let a=2 in a-1"))
//    assert(4 == eval("let a=2 in let b=3 in let a=1 in a+b"))
  }
}
