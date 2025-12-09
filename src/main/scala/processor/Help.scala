package processor

import archi.ArchiException

import scala.language.implicitConversions


enum Help(_message: String, withInputArg: Boolean = true):

  val message: String =
    val values = this :: Option.when(withInputArg)("<archi-path>").toList ::: _message :: Nil
    values.mkString(" ")

  def exception(prefix: String = "Invalid args:"): ArchiException =
    ArchiException(s"$prefix\n$message")

  
  case findModule         extends Help("[<keys>]")
  case moduleProjects     extends Help("[<modules>]")
  case getPath            extends Help("<module1> <module2>")
  case moduleProjectDiff  extends Help("<module1> <module2>")
  case archiDiff          extends Help("<archi1> <archi2>", withInputArg = false)
  case rewrite            extends Help("[-i <archi-input>] [-o <archi-output>] [-l <archiDiff-log>] -r <rules-input>",
    withInputArg = false)
  case help               extends Help("[<function-name>]", withInputArg = false)
