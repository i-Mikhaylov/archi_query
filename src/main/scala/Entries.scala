import archi.{Archi, ArchiException}
import processor.{Help, Rewriter}
import util.Printer
import util.Printer.printError

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.sys.process.{Process, stdin}
import scala.util.CommandLineParser.FromString
import scala.util.Try


private object PathInput:
  def get(arg: String): String = arg match
    case "-" => String(stdin.readAllBytes)
    case s"$$($command)" => Process(command).!!
    case path => Files.readString(Paths.get(arg))

private object ArchiInput:
  def unapply(path: String): Some[Archi] = Some(Archi(PathInput.get(path)))


private def currentHelp: Help = Help valueOf Thread.currentThread.getStackTrace.last.getClassName
private def invalidArgs: ArchiException = currentHelp.exception()
private def invalidArgs(prefix: String): ArchiException = currentHelp.exception(prefix)


val _ = Thread.setDefaultUncaughtExceptionHandler {
  case (_, error: ArchiException) => printError(error.getMessage)
  case (_, error) => error.printStackTrace()
}



@main private def findModule(args: String*): Unit = args.toList match
  case Nil => throw invalidArgs
  case _ :: Nil => throw invalidArgs("No key specified to find")
  case ArchiInput(archi) :: nameSubstrings => processor.findModule(nameSubstrings)(archi)


@main private def moduleProjects(args: String*): Unit = args.toList match
  case Nil => throw invalidArgs
  case _ :: Nil => throw invalidArgs("No modules specified")
  case ArchiInput(archi) :: moduleNames => processor.moduleProjects(moduleNames.map(archi.byName))


@main private def moduleDiff(args: String*): Unit = args.toList match
  case ArchiInput(archi) :: module1 :: module2 :: Nil =>
    import archi.nameToNode
    processor.moduleDiff(module1, module2)
  case _ => throw invalidArgs


//noinspection AccessorLikeMethodIsUnit
@main private def getPath(args: String*): Unit = args.toList match
  case ArchiInput(archi) :: module1 :: module2 :: Nil =>
    import archi.nameToNode
    Archi.getPath(module1, module2).foreach(println)
  case _ => throw invalidArgs


@main private def projectDiff(args: String*): Unit = args.toList match
  case ArchiInput(src) :: ArchiInput(dst) :: ignoreModules => processor.projectDiff(src, dst, ignoreModules.toSet)
  case _ => throw invalidArgs


@main private def rewrite(args: String*): Unit =
  val argMap = args
    .toList
    .sliding(2, 2)
    .collect { case List(key, value) => key -> value }
    .toList
    .groupMap(_._1)(_._2)
    .map {
      case (key, value :: Nil) => key -> value
      case (key, values) => throw invalidArgs(s"Duplicate values for $key")
    }
    .withDefaultValue("-")

  def writeOutput(path: String, defaultPrint: String => Unit) =
    if path == "-" then defaultPrint(_)
    else Files.writeString(Paths get path, _)

  val srcArchi = ArchiInput.unapply(argMap("-i")).value
  val rules = PathInput get argMap.getOrElse("-r", throw invalidArgs)
  val printOutput: String => Unit = argMap("-o") match
    case "-" => println
    case path => Files.writeString(Paths get path, _)
  val printInfo: String => Unit = argMap("-i") match
    case "-" => Printer.printInfo
    case path => Files.writeString(Paths get path, _)

  val (rewritten, manuallyChanged) = Rewriter.parseRewrite(srcArchi, rules)
  printOutput(rewritten.toString)
  processor.projectDiff(srcArchi, rewritten, manuallyChanged.toSet)


@main private def help(args: String*): Unit =
  def getHelp(name: String) =
    Try(Help.valueOf(name))
      .recover { case _ => throw ArchiException(s"Invalid name: $name") }
      .get
  for help <- if args.isEmpty then Help.values.toSeq else args.map(getHelp)
  do println(help.message)
