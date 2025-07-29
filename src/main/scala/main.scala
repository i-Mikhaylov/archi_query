import Printer.printError

import java.io.File
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.sys.process.Process


private val archiDst = Paths.get("../c4enterprise/uses.archimate")
private lazy val srcArchi = Archi(Process("git show HEAD:uses.archimate", new File("../c4enterprise/")).!!)
private lazy val dstArchi = Archi(Files.readString(archiDst))


private class ArchiException(message: String) extends Exception(message)
object ArchiException:
  def apply(message: String) = new ArchiException(message)


def findPrintModules(nameSubstring: String*): Unit =
  val substrings = nameSubstring.toList
  if (substrings.isEmpty) printError("No key specified to find")
  else dstArchi.byId.values
    .map(_.name)
    .filter(name => substrings.exists(name.contains))
    .toList
    .sorted
    .foreach(println)


def printModuleProjects(modules: Node*): Unit =
  def processModule(module: Node, prefix: String): Unit =
    module.targetProjects.toList.map(prefix + _.name).sorted.foreach(println)
  modules.toList match
    case Nil => printError("No modules specified")
    case single :: Nil => processModule(single, "")
    case multiple =>
      for module <- modules yield
        println(s"\n${module.name}:")
        processModule(module, "  ")
        println()


def printModuleProjectDiff(module1: Node, module2: Node): Unit =
  val List(projects1, projects2) = List(module1, module2).map(_.targetProjects)
  def print(module: Node, selfProjects: Set[Node], otherProjects: Set[Node]): Unit =
    println(s"Only ${module.name}:")
    selfProjects.view.filterNot(otherProjects.contains).map("  " + _.name).toList.sorted.foreach(println)
  print(module1, projects1, projects2)
  print(module2, projects2, projects1)


def printInvalid(): Unit =
  val nodes = srcArchi.byId.values
  def byName(names: String*) = names.map(srcArchi.byName)

  def sourceFilter(src: Iterable[Node], filter: Node => Boolean) = for
    node <- src.view
    source <- node.sources
    if filter(source)
  yield source -> node

  val mainDomain = byName("c4.core.inheritance", "c4.mod.domain.cargo")
  val mainCargo = byName("c4.cargo.placement", "c4.cargo.base", "c4.cargo.decision", "c4.cargo.strategy", "c4.cargo.withdraw").toSet
  val mainSrl = byName("c4.mod.c4srl.base", "c4.mod.wtms.c4cargo")
  val allMain = mainCargo ++ mainSrl ++ mainDomain

  val modDomain = byName(
    "c4.mod.domain.breakbulkcargo",
    "c4.mod.domain.c4cnt.base",
    "c4.mod.domain.c4gencargo",
    "c4.mod.domain.c4maficargo",
    "c4.mod.domain.c4perscar",
    "c4.mod.domain.c4roadunit",
    "c4.mod.domain.rwcar_base",
    "c4.mod.domain.tug",

    "c4.mod.domain.railway",
  ).toSet
  val modCargo = nodes.filter(node => node.name.startsWith("c4.cargo") && !modDomain.contains(node) && !allMain.contains(node)).toSet
  val modWtms = byName(
    "c4.mod.wtms.c4bulkcargo",
    "c4.mod.wtms.c4vessel.cnt",
    "c4.mod.wtms.c4accessibility.cnt",
    "c4.mod.wtms.c4docker.forcemove.gencargo",
    "c4.mod.wtms.c4docker.gencargo",
    "c4.mod.wtms.c4docker.location.correction.gencargo",
    "c4.mod.wtms.c4maficargo",
    "c4.mod.wtms.c4perscar",
    "c4.mod.wtms.c4railway",
  ).toSet
  val mod = modDomain ++ modCargo ++ modWtms

  val modToMain       = sourceFilter(allMain,   mod.contains)
  val mainToModDomain = sourceFilter(modDomain, (mainCargo ++ mainSrl).contains)
  val mainToModCargo  = sourceFilter(modCargo,  mainSrl.contains)
  val modToModDomain  = sourceFilter(modDomain, mod.contains)
  val modToModCargo   = sourceFilter(modCargo,  (modCargo ++ modWtms).contains)

  (modToMain ++ mainToModDomain ++ mainToModCargo ++ modToModDomain ++ modToModCargo)
    .map((source, target) => s"""//"${source.name}" -> "${target.name}",""")
    .toList
    .sorted
    .foreach(println)


def printProjectsDiff(ignoreModuleName: String*): Unit =
  val ignoreModuleNames = ignoreModuleName.toSet
  for projectId <- (srcArchi.projects ::: dstArchi.projects).distinct.sortBy(_.name).map(_.id) yield

    val List((srcProject, srcModules), (dstProject, dstModules)) = List(srcArchi, dstArchi)
      .map(_.byId.get(projectId).filter(_.isProject))
      .map { project => project -> project.map(_.allSources).getOrElse(Set.empty) }

    val onlySrcModules = srcModules.view.filter(n => !dstModules.contains(n) && !ignoreModuleNames.contains(n.name)).toList
    val onlyDstModules = dstModules.view.filter(n => !srcModules.contains(n) && !ignoreModuleNames.contains(n.name)).toList
    if (onlySrcModules.nonEmpty || onlyDstModules.nonEmpty)
      println((dstProject orElse srcProject).get.name + ":")
      if (onlySrcModules.nonEmpty) println("  - " + onlySrcModules.map(_.name).sorted.mkString("\n    "))
      if (onlyDstModules.nonEmpty) println("  + " + onlyDstModules.map(_.name).sorted.mkString("\n    "))


def rewrite(
  toRenameModules: List[(String, String)] = Nil,
  toAddModules: List[String] = Nil,
  toRemoveDependencies: List[(String, String)] = Nil,
  toAddDependencies: List[(String, String)] = Nil,
): Unit =
  srcArchi match { case archi =>
    if (toRenameModules.nonEmpty) archi.renameModules(toRenameModules.toMap) else archi
  }
  match { case archi =>
    if (toAddModules.nonEmpty) archi.addModules(toAddModules) else archi
  }
  match { case archi =>
    val toRemoveIds = toRemoveDependencies.map((from, to) => archi.byName(from) -> archi.byName(to))
    if (toRemoveIds.nonEmpty) archi.removeDependencies(toRemoveIds) else archi
  }
  match { case archi =>
    val toAddIds = toAddDependencies.map((from, to) => archi.byName(from) -> archi.byName(to))
    if (toAddIds.nonEmpty) archi.addDependencies(toAddIds) else archi
  }
  match { case archi =>
    Files.writeString(archiDst, archi.toString)
  }


def parseRewrite(data: String): Unit =

  abstract class AnyMode
  abstract class Mode(val key: String) extends AnyMode
  case object NoMode extends AnyMode
  case object RenameModules extends Mode("renamemodules")
  case object AddModules extends Mode("addmodules")
  case object RemoveDeps extends Mode("removedependencies")
  case object AddDeps extends Mode("adddependencies")

  object ModeLine:
    def unapply(line: String): Option[Mode] =
      val key = line.filter(_.isLetter).toLowerCase
      List(RenameModules, AddModules, RemoveDeps, AddDeps).find(_.key == key)
  object ArrowLine:
    private val regex = "(.*?)\\s*->\\s*(.*?)".r
    def unapply(line: String): Option[(String, String)] =
      Some(line).collect { case regex(module1, module2) => module1 -> module2 }

  type LS = List[String]
  type LSS = List[(String, String)]
  
  @tailrec def parse(
    preParsedLines: LS,
    renameModules: LSS = Nil,
    addModules: LS = Nil,
    removeDeps: LSS = Nil,
    addDeps: LSS = Nil,
    mode: AnyMode = NoMode,
  ): (LSS, LS, LSS, LSS) = (mode, preParsedLines) match
    case (_, Nil)                               => (renameModules, addModules, removeDeps, addDeps)
    case (_, ModeLine(newMode) :: tail)         => parse(tail, renameModules, addModules, removeDeps, addDeps, newMode)
    case (RenameModules,ArrowLine(rename)::tail)=> parse(tail, rename :: renameModules, addModules, removeDeps, addDeps, mode)
    case (AddModules, module :: tail)           => parse(tail, renameModules, module :: addModules, removeDeps, addDeps, mode)
    case (RemoveDeps, ArrowLine(dep) :: tail)   => parse(tail, renameModules, addModules, dep :: removeDeps, addDeps, mode)
    case (AddDeps, ArrowLine(dep) :: tail)      => parse(tail, renameModules, addModules, removeDeps, dep :: addDeps, mode)
    case (_, invalid :: _)                      => throw ArchiException(s"Invalid line: $invalid")

  val uncommentRegex = "(.*?)(//|#).*".r
  val trimRegex = "\\s*(.*?)\\s*".r
  val preParsedData = data.split('\n').toList
    .map {
      case uncommentRegex(uncommentedLine, _) => uncommentedLine
      case noComment => noComment
    }
    .collect { case trimRegex(trimmed) if trimmed.nonEmpty => trimmed }

  val (renameModules, addModules, removeDeps, addDeps) = parse(preParsedData)
  rewrite(renameModules, addModules, removeDeps, addDeps)
  printProjectsDiff(addModules.toArray*)


@main def main(allArgs: String*): Unit =
  try
    val (archi, queryArgs) = allArgs match
      case Nil => throw ArchiException("No args")
      case "src" :: tail => srcArchi -> tail
      case "dst" :: tail => dstArchi -> tail
      case _ => srcArchi -> allArgs
    import archi.nameToNode

    queryArgs match
      case "find-module" :: keys                      => findPrintModules(keys*)
      case "module-projects" :: moduleNames           => printModuleProjects(moduleNames.map(nameToNode)*)
      case "module-diff" :: module1 :: module2 :: Nil => printModuleProjectDiff(module1, module2)
      case "module-path" :: module1 :: module2 :: Nil => Archi.getPath(module1, module2).foreach(println)
      case "get-invalid" :: Nil                       => printInvalid()
      case "rewrite" :: dataPath :: Nil               => parseRewrite(Files.readString(Paths get dataPath))
      case "module-diff" :: _ => throw ArchiException("module-diff needs exactly 2 arguments")
      case "module-path" :: _ => throw ArchiException("module-path needs exactly 2 arguments")
      case "get-invalid" :: _ => throw ArchiException("get-invalid doesn't need any extra arguments")
      case "rewrite" :: _     => throw ArchiException("rewrite needs only 1 argument with the path of data file")
      case _                  => throw ArchiException("Invalid command")

  catch case archi: ArchiException => printError(archi.getMessage)
