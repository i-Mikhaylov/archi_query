package processor

import archi.{Archi, ArchiException, Node}
import processor.Modes.{AnyMode, NoMode, modeByKey, *}
import util.allSealedObjects

import scala.annotation.tailrec
import scala.language.implicitConversions


private object Modes:
  private type Process = (Archi, List[String]) => Archi
  private val arrowRegex = "(.*?)\\s?->\\s?(.*?)".r
  private def throwLine(line: String): Nothing = throw ArchiException(s"Invalid line: $line")
  
  private implicit def toNode(name: String)(using archi: Archi): Node = archi.byName(name)
  private implicit def toPair[N1, N2](name: String)(using archi: Archi, conv1: String => N1, conv2: String => N2): (N1, N2) =
    name match
      case arrowRegex(source, target) => (conv1(source), conv2(target))
      case invalid => throwLine(invalid)
  private implicit def toList[N](names: List[String])(using archi: Archi, conv: String => N): List[N] = names.map(conv)

  sealed abstract class AnyMode(archiTransform: Process):
    def process(archi: Archi, lines: List[String]): Archi =
      if (lines.isEmpty) archi
      else archiTransform(archi, lines)
  sealed abstract class Mode(val key: String, archiTransform: Process) extends AnyMode(archiTransform)

  object NoMode       extends AnyMode((archi, lines) => lines.headOption.fold(archi)(throwLine))
  object RemoveModule extends Mode("remove modules",      { implicit (archi, names) => archi.removeModules(names) })
  object RenameModule extends Mode("rename modules",      { implicit (archi, names) => archi.renameModules(names) })
  object AddModule    extends Mode("add modules",         { implicit (archi, names) => archi.addModules(names.reverse) })
  object RemoveDep    extends Mode("remove dependencies", { implicit (archi, deps) => archi.removeDependencies(deps) })
  object AddDep       extends Mode("add dependencies",    { implicit (archi, deps) => archi.addDependencies(deps.reverse) })
  object SuperProject extends Mode("super project",       {
    case (archi, Nil) => archi
    case (archi, superProject :: Nil) =>
      val superNode = archi.byName(superProject)
      val deps = archi.projects.filter(_ != superNode).map(_ -> superNode)
      archi.addDependencies(deps)
    case (_, multiple) => throw ArchiException("Found multiple super projects: " + multiple.mkString("; "))
  })
  
  val allModes: List[AnyMode] = NoMode :: allSealedObjects[Mode]
  lazy val modeByKey: Map[String, Mode] = allModes.collect { case mode: Mode => mode.key -> mode }.toMap.withDefault(throwLine)


object Rewriter:
  private val uncommentRegex = "(.*?)(//|#).*".r
    
  @tailrec private def parseRewrite(
    archi: Archi,
    preParsedLines: List[String],
    mode: AnyMode = NoMode,
    modeLines: List[String] = Nil,
  ): Archi = preParsedLines match
    case s"$modeKey:" :: tail => parseRewrite(mode.process(archi, modeLines), tail, modeByKey(modeKey.toLowerCase))
    case line :: tail => parseRewrite(archi, tail, mode, line :: modeLines)
    case Nil => mode.process(archi, modeLines)
  
  def parseRewrite(archi: Archi, data: String): (Archi, List[String]) =
    val preParsedData = data.split('\n').toList
      .map {
        case uncommentRegex(uncommentedLine, _) => uncommentedLine
        case noComment => noComment
      }
      .map(_.replaceAll("\\s+", " ").trim)
      .filter(_.nonEmpty)
    parseRewrite(archi, preParsedData) -> preParsedData.filter(line => !line.contains("->") && !line.endsWith(":"))
