package util

import scala.io.AnsiColor


object Printer:
  private def coloredPrint(message: String, ansiColor: String): Unit =
    Console.err.println(ansiColor + message + Console.RESET)
  def printInfo (message: String): Unit = coloredPrint(message, AnsiColor.BLACK)
  def printWarn (message: String): Unit = coloredPrint(message, AnsiColor.YELLOW)
  def printError(message: String): Unit = coloredPrint(message, AnsiColor.RED)
