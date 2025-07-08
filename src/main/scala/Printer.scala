import scala.io.AnsiColor


object Printer:
  private def coloredPrint(message: String, ansiColor: String): Unit =
    println(ansiColor + message + Console.RESET)
  def printSuccess(message: String): Unit = coloredPrint(message, AnsiColor.GREEN)
  def printWarn   (message: String): Unit = coloredPrint(message, AnsiColor.YELLOW)
  def printError  (message: String): Unit = coloredPrint(message, AnsiColor.RED)
