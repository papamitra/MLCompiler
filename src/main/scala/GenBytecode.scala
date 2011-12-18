
//package localhost.ml

import localhost.ml._
import ch.epfl.lamp.fjbg._

class Code(cname:String) {
  val context = new FJBGContext()
  import JAccessFlags._
  val jclass = context.JClass(ACC_PUBLIC,
    cname,
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

}

object parser extends MLParser {
  def parse(args: String) = {
    parseAll(expr, args)
  }
}

object Main extends App {

  import scala.tools.nsc.interpreter._
  import scala.tools.nsc.io.{ AbstractFile, VirtualDirectory }

  val vd = new VirtualDirectory("(memory)", None)
  val parent = Thread.currentThread.getContextClassLoader
  //  val parent = null
  val afcl = new AbstractFileClassLoader(vd, parent)
  //  Thread.currentThread.setContextClassLoader(afcl)

  var i = 0
  while (true) {
    val line = Console.readLine()
    if (line == ":quit") sys.exit

    try {
      val ast = parser.parse(line).get

      val className = "$line%d".format(i)
      i += 1
      val file = vd.fileNamed(className + ".class")
      val outstream = new java.io.DataOutputStream(file.bufferedOutput)

      val code = new Code(className)
      import code._
      ast.emit(maincode)
      maincode.emitIRETURN()

      jclass writeTo outstream
      outstream.close()

      val hello = afcl.loadClass(className)
      val thread = afcl.loadClass("java.lang.Thread")
      val method = hello.getMethods.filter(_.getName == "run").head
      println(method.invoke(null, null))
    } catch {
      case e => println(e.getStackTraceString)
    }
  }
}
