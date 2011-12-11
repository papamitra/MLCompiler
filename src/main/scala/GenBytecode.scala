
//package localhost.ml

import localhost.ml._
import ch.epfl.lamp.fjbg._

object Main extends App {
  val context = new FJBGContext()
  import JAccessFlags._
  val jclass = context.JClass(ACC_PUBLIC,
   "Hello",
    "java/lang/Object",
    JClass.NO_INTERFACES,
    "")

  // コンストラクタ生成
  val initMethod = jclass.addNewMethod(ACC_PUBLIC,
    "<init>",
    JType.VOID,
    JType.EMPTY_ARRAY,
    new Array[String](0))

  val init = initMethod.getCode().asInstanceOf[JExtendedCode]
  init.emitALOAD_0
  init.emitINVOKESPECIAL("java/lang/Object",
    "<init>",
    JMethodType.ARGLESS_VOID_FUNCTION)
  init.emitRETURN()

  // main生成
  val mainMethod =
    jclass.addNewMethod(ACC_PUBLIC | ACC_STATIC,
      "main",
      JType.VOID,
      Array(new JArrayType(new JObjectType("java.lang.String"))),
      Array("args"))

  val maincode = mainMethod.getCode().asInstanceOf[JExtendedCode]

  maincode.emitGETSTATIC("java.lang.System", "out", new JObjectType("java.io.PrintStream"))
  maincode.emitLDC("Hello,World")
  maincode.emitINVOKEVIRTUAL("java.io.PrintStream",
    "println",
    new JMethodType(JType.VOID, Array(new JObjectType("java.lang.String"))))
  maincode.emitRETURN()

  import scala.tools.nsc.interpreter._  
  import scala.tools.nsc.io.{ AbstractFile, VirtualDirectory }

  val vd = new VirtualDirectory("(memory)", None)
  val parent = Thread.currentThread.getContextClassLoader
  val afcl = new AbstractFileClassLoader(vd, parent)
  Thread.currentThread.setContextClassLoader(afcl)

  val file = vd.fileNamed("Hello.class")

  val outstream = new java.io.DataOutputStream(file.bufferedOutput)
  jclass writeTo outstream
  outstream.close()

//  val hello = Class.forName("Hello")
  val hello = afcl.loadClass("Hello")
  val thread = afcl.loadClass("java.lang.Thread")
  val method = hello.getMethods.filter(_.getName == "main").head
  method.invoke(null, null)

}
