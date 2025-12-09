package util

import scala.io.AnsiColor


implicit class ColoredString(str: String):
  def colored(color: AnsiColor => String): String =
    color(AnsiColor) + str + AnsiColor.RESET
