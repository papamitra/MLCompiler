
package localhost.ml

object Main extends App {
  var i = 0
  while (true) {
    val line = Console.readLine()
    if (line == ":quit") sys.exit

    try {
      val className = "$line%d".format(i)
      i += 1
      val code = new GenJVM(className, line)
      println(code.eval)
    } catch {
      case e => println(e.getStackTraceString)
    }
  }
}
