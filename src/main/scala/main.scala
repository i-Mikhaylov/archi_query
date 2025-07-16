import java.nio.file.{Files, Paths}
import scala.annotation.tailrec


private val archiSrc = "renamed.archimate"
private val archiDst = "../c4enterprise/uses.archimate"

private val toAddModules = List[String](
  //"c4.mod.domain.decision",                                   mb divide and move some part to domain?
)

private val toRemoveDependencies = List[(String, String)](
  //modToMain:
  //"c4.cargo.operationalparameters" -> "c4.cargo.base",        to the end (too many deps but I need to try)

  //mainToModDomain:
  "c4.cargo.base" -> "c4.mod.domain.c4maficargo",
  "c4.cargo.base" -> "c4.mod.domain.tug",
  /*"c4.cargo.decision" -> "c4.mod.domain.c4cnt.base",          mb divide and move some part to domain?
  "c4.cargo.decision" -> "c4.mod.domain.c4gencargo",
  "c4.cargo.decision" -> "c4.mod.domain.c4maficargo",
  "c4.cargo.decision" -> "c4.mod.domain.c4perscar",
  "c4.cargo.decision" -> "c4.mod.domain.c4roadunit",
  "c4.cargo.decision" -> "c4.mod.domain.tug",*/
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4cnt.base",
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4maficargo",
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4roadunit",
  //"c4.mod.c4srl.base" -> "c4.mod.domain.tug",                 Capacity in srl?
  "c4.mod.domain.c4placement" -> "c4.mod.domain.c4cnt.base",
  "c4.mod.domain.c4placement" -> "c4.mod.domain.c4gencargo",
  "c4.mod.domain.c4placement" -> "c4.mod.domain.c4perscar",
  "c4.mod.domain.c4placement" -> "c4.mod.domain.c4roadunit",
  //"c4.mod.domain.c4placement" -> "c4.mod.domain.rwcar_base",  merge domain.rwcar_base into cargo.rwcar ?

  //mainToModCargo:
  "c4.mod.c4srl.base" -> "c4.cargo.c4gencargo",
  //"c4.mod.c4srl.base" -> "c4.cargo.imoresolution.base",       depends hardly
  "c4.mod.c4srl.base" -> "c4.cargo.oogresolution",

  //modToModDomain:
  "c4.cargo.imo" -> "c4.mod.domain.c4cnt.base",

  //modToModCargo:
  "c4.cargo.c4container.base" -> "c4.cargo.tiers_in_bundle",
  //"c4.mod.wtms.c4bulkcargo" -> "c4.cargo.c4customs",          depends hardly
  "c4.mod.wtms.c4docker.gencargo" -> "c4.cargo.tiers_in_bundle",
)
private val toAddDependencies = List[(String, String)](
  "c4.mod.domain.c4techflow" -> "c4.mod.domain.c4cnt.base",
  "c4.mod.reports.cargo" -> "c4.mod.domain.c4maficargo",
  "c4.mod.domain.c4placement" -> "c4.cargo.c4perscar",
  "c4.mod.domain.c4placement" -> "c4.cargo.c4gencargo",
  "c4.cargo.strategy" -> "c4.cargo.c4gencargo",
  "c4.cargo.withdraw" -> "c4.cargo.c4gencargo",
  "c4.cargo.decision" -> "c4.cargo.imoresolution.base",
  "c4.mod.domain.c4cargospec.images" -> "c4.mod.domain.c4truckcallreport",
)


private lazy val srcArchi = Archi(Files.readString(Paths.get(archiSrc)))
private lazy val dstArchi = Archi(Files.readString(Paths.get(archiDst)))


def findPrintModules(nameSubstring: String): Unit =
  dstArchi.byId.values
    .map(_.name)
    .filter(_.contains(nameSubstring))
    .toList
    .sorted
    .foreach(println)


def printModuleProjects(moduleNames: String*): Unit =
  for moduleName <- moduleNames yield
    val module = srcArchi.byName(moduleName)
    println()
    srcArchi.projects
      .filter(srcArchi.allSources(_).contains(module))
      .map(_.name)
      .sorted
      .foreach(println)


def printInvalid(): Unit =
  val nodes = dstArchi.byId.values
  def byName(names: String*) = names.map(dstArchi.byName)

  def sourceFilter(src: Iterable[Node], filter: Node => Boolean) = for
    node <- src.view
    source <- node.sources
    if filter(source)
  yield source -> node

  val mainDomain = byName("c4.core.inheritance", "c4.mod.domain.cargo")
  val mainCargo = byName("c4.mod.domain.c4placement", "c4.cargo.base", "c4.cargo.decision", "c4.cargo.strategy", "c4.cargo.withdraw").toSet
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


def printProjectsDiff(): Unit =

  @tailrec def allSources(nodes: List[Node], acc: Set[Node] = Set.empty): Set[Node] =
    val newNodes = nodes.filterNot(acc.contains)
    if (newNodes.isEmpty) acc
    else allSources(nodes.flatMap(_.sources), acc ++ newNodes)

  val List(srcProjects, dstProjects) = for archi <- List(srcArchi, dstArchi) yield
    archi.projects.map { project => project -> allSources(project :: Nil) }.toMap

  for project <- (srcArchi.projects ::: dstArchi.projects).distinct.sortBy(_.name) yield
    val srcModules = srcProjects.getOrElse(project, Set.empty)
    val dstModules = dstProjects.getOrElse(project, Set.empty)
    val onlySrcModules = srcModules.view.filterNot(dstModules.contains).toList
    val onlyDstModules = dstModules.view.filterNot(srcModules.contains).toList
    if (onlySrcModules.nonEmpty || onlyDstModules.nonEmpty)
      println(project.name + ":")
      if (onlySrcModules.nonEmpty) println("  - " + onlySrcModules.map(_.name).sorted.mkString("\n    "))
      if (onlyDstModules.nonEmpty) println("  + " + onlyDstModules.map(_.name).sorted.mkString("\n    "))


def rewrite(): Unit =
  val byName = srcArchi.byName

  val toRemoveIds = toRemoveDependencies
    .map((fromName, toName) => byName(fromName) -> byName(toName))
  val toAddIds = toAddDependencies
    .map((fromName, toName) => byName(fromName) -> byName(toName))

  val updated = Some(srcArchi)
    .map { archi => if (toAddModules.nonEmpty) archi.addModules(toAddModules) else archi }
    .map { archi => if (toRemoveIds.nonEmpty) archi.removeDependencies(toRemoveIds) else archi }
    .map { archi => if (toAddIds.nonEmpty) archi.addDependencies(toAddIds) else archi }
    .get
  Files.writeString(Paths.get(archiDst), updated.toString)


@main def main(): Unit =
  import srcArchi.nameToNode

  //println(srcArchi.moduleProjectDiff("c4.mod.domain.rwcar", "c4.mod.domain.rwcar_base"))
  //srcArchi.getPath("c4.mod.domain.c4cargospec.images", "c4.mod.domain.c4truckcallreport").foreach(println)
  //printModuleProjects("c4.cargo.oogresolution")
  //findPrintModules("service")
  rewrite();  printProjectsDiff()
  printInvalid()
