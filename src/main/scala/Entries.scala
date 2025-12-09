import archi.{Archi, ArchiException}
import processor.{Help, Rewriter}

import java.io.{FileWriter, PrintStream}
import java.nio.file.{Files, Paths}
import scala.Console
import scala.io.AnsiColor
import scala.language.implicitConversions
import scala.sys.process.{Process, stdin}
import scala.util.CommandLineParser.FromString
import scala.util.Try
import util.ColoredString


private object PathInput:
  def get(arg: String): String = arg match
    case "-" => String(stdin.readAllBytes)
    case s"$$($command)" => Process(command).!!
    case path => Files.readString(Paths.get(arg))

private object ArchiInput:
  def unapply(path: String): Some[Archi] = Some(Archi(PathInput.get(path)))


private def currentHelp = Help valueOf Thread.currentThread.getStackTrace.last.getClassName
private def invalidArgs = currentHelp.exception()
private def invalidArgs(prefix: String) = currentHelp.exception(prefix)


private def process(args: Seq[String])(func: PartialFunction[List[String], Iterable[Any]]): Unit =
  val fullFunc = func.andThen { _.foreach(println) }.orElse { case _ => throw invalidArgs }
  try fullFunc(args.toList)
  catch case error: ArchiException => Console.err.println(error.getMessage)



@main private def findModule(args: String*): Unit = process(args):
  case _ :: Nil => throw invalidArgs("No key specified to find")
  case ArchiInput(archi) :: nameSubstrings => processor.findModule(nameSubstrings)(archi)


@main private def moduleProjects(args: String*): Unit = process(args):
  case _ :: Nil => throw invalidArgs("No modules specified")
  case ArchiInput(archi) :: moduleNames => moduleNames.map(archi.byName).flatMap(processor.moduleProjects)


//noinspection AccessorLikeMethodIsUnit
@main private def getPath(args: String*): Unit = process(args):
  case ArchiInput(archi) :: module1 :: module2 :: Nil =>
    import archi.nameToNode
    Archi.getPath(module1, module2)


@main private def moduleProjectDiff(args: String*): Unit = process(args):
  case ArchiInput(archi) :: module1 :: module2 :: Nil =>
    import archi.nameToNode
    processor.moduleProjectDiff(module1, module2)


@main private def archiDepDiff(args: String*): Unit = process(args):
  case ArchiInput(src) :: ArchiInput(dst) :: Nil =>
    val lines = processor.archiDepDiff(src, dst)
    if lines.nonEmpty then lines else "All deps are equal".colored(_.GREEN) :: Nil


@main private def archiDiff(args: String*): Unit = process(args):
  case ArchiInput(src) :: ArchiInput(dst) :: Nil =>
    val lines = processor.archiDiff(src, dst)
    if lines.nonEmpty then lines else "All projects' contents are equal".colored(_.GREEN) :: Nil


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

  val srcArchi = ArchiInput.unapply(argMap("-i")).value
  val rules = PathInput get argMap.getOrElse("-r", throw invalidArgs)
  val printOutput: String => Unit =
    argMap("-o") match      //Can be called only once
      case "-" => Console.out.println
      case path => Files.writeString(Paths get path, _)
  val (rewritten, manuallyChanged) = Rewriter.parseRewrite(srcArchi, rules)
  printOutput(rewritten.toString)

  lazy val infoOut = argMap.get("-l") match
    case None => Console.err.print(AnsiColor.WHITE); Console.err
    case Some("-") => Console.out
    case Some(path) => new FileWriter(path)
  for line <- processor.archiDiff(srcArchi, rewritten, manuallyChanged.toSet) do
    infoOut.append(line).append(System.lineSeparator)
  infoOut match
    case file: FileWriter => file.close()
    case print => print.append(AnsiColor.RESET)


@main private def help(args: String*): Unit = process(args):
  case Nil => Help.values
  case names => for name <- names yield
    Try(Help.valueOf(name))
      .recover { case _ => throw ArchiException(s"Invalid name: $name") }
      .get
