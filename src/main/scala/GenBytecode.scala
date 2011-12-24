
package localhost.ml

import ch.epfl.lamp.fjbg._

case class Context(val code: JExtendedCode, val method:JMethod, val env:List[JLocalVariable])

object GenJVM {
  import scala.tools.nsc.interpreter.AbstractFileClassLoader
  import scala.tools.nsc.io.{ AbstractFile, VirtualDirectory }

  val vd = new VirtualDirectory("(memory)", None)
  private val parent = Thread.currentThread.getContextClassLoader
  //  val parent = null
  val classLoader = new AbstractFileClassLoader(vd, parent)
  //  Thread.currentThread.setContextClassLoader(afcl)

  object parser extends MLParser
  def parse(args: String) = {
    parser.parseAll(parser.expr, args)
  }

  def write(jclass: ch.epfl.lamp.fjbg.JClass) {
    val file = vd.fileNamed(jclass.getName + ".class")
    val outstream = new java.io.DataOutputStream(file.bufferedOutput)
    jclass writeTo outstream
    outstream.close()
  }

  def emit(expr: Expr, ctx: Context) {
    import ctx._
    expr match {
      case IntLiteral(v) => code.emitLDC(v)
      case IfExpr(p, t, f) =>
        val fail = code.newLabel()
        val ret = code.newLabel()
        emit(p, ctx)
        code.emitIFEQ(fail)
        emit(t, ctx)
        code.emitGOTO(ret)
        fail.anchorToNext()
        emit(f, ctx)
        ret.anchorToNext()
      case BinOp(op, lhs, rhs) =>
        emit(lhs, ctx)
        emit(rhs, ctx)
        op match {
          case Add => code.emitIADD()
          case Sub => code.emitISUB()
          case Mul => code.emitIMUL()
          case Less =>
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
      case BoolLiteral(b) => b match{
	case true => code.emitLDC(1)
	case false => code.emitLDC(0)
      }
      case Symbol(name) =>
	val variable = env.find(_.getName == name).get
	code.emitLOAD(variable)
      case LetExpr(sym @ Symbol(symName), defcode, expr) =>
	emit(defcode, ctx)
	val variable = method.addNewLocalVariable(JType.INT, symName)
	code.emitSTORE(variable)
	emit(expr, ctx.copy(env = variable :: ctx.env))
    }
  }
}

class GenJVM(className: String, src: String) {
  import GenJVM._

  val context = new FJBGContext()
  import JAccessFlags._
  val jclass = context.JClass(ACC_PUBLIC,
    className,
    "java/lang/Object",
    JClass.NO_INTERFACES,
    "")

  val mainMethod =
    jclass.addNewMethod(ACC_PUBLIC | ACC_STATIC,
      "run",
      JType.INT,
      Array(new JArrayType(new JObjectType("java.lang.String"))),
      Array("args"))

  val maincode = mainMethod.getCode().asInstanceOf[JExtendedCode]

  val ast = parse(src).get

  val eval: Any = {
    emit(ast, new Context(maincode, mainMethod, Nil))
    maincode.emitIRETURN()

    write(jclass)

    val klass = classLoader.loadClass(className)
    val method = klass.getMethods.filter(_.getName == "run").head
    method.invoke(null, null)
  }
}
