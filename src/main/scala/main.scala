import java.io.File
import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.sys.process.Process


private val archiDst = Paths.get("../c4enterprise/uses.archimate")

private val toAddModules = List[String](
  "c4.mod.domain.decision", //                                  mb divide and move some part to domain?
)

private val toRemoveDependencies = List[(String, String)](

  //modToMain:
  //"c4.cargo.operationalparameters" -> "c4.cargo.base",        to the end (too many deps but I need to try)

  //mainToModDomain:
  "c4.cargo.decision" -> "c4.mod.domain.c4cnt.base",//          mb divide and move some part to domain?
  "c4.cargo.decision" -> "c4.mod.domain.c4gencargo",
  "c4.cargo.decision" -> "c4.mod.domain.c4maficargo",
  "c4.cargo.decision" -> "c4.mod.domain.c4perscar",
  "c4.cargo.decision" -> "c4.mod.domain.c4roadunit",
  "c4.cargo.decision" -> "c4.mod.domain.tug",
  //"c4.cargo.decision" -> "c4.mod.domain.rwcar",
  //"c4.mod.c4srl.base" -> "c4.mod.domain.tug",                 Capacity in srl?
  //"c4.mod.domain.c4placement" -> "c4.mod.domain.rwcar_base",  merge domain.rwcar_base into cargo.rwcar ?

  //mainToModCargo:
  //"c4.mod.c4srl.base" -> "c4.cargo.imoresolution.base",       depends hardly

  //modToModDomain:

  //modToModCargo:
  //"c4.mod.wtms.c4bulkcargo" -> "c4.cargo.c4customs",          depends hardly
)
private val toAddDependencies = List[(String, String)](
  "c4.mod.domain.cargo" -> "c4.mod.domain.decision",
  "c4.mod.domain.decision" -> "c4.cargo.decision",
  "c4.mod.domain.decision" -> "c4.mod.domain.c4cnt.base",
  "c4.mod.domain.decision" -> "c4.mod.domain.c4gencargo",
  "c4.mod.domain.decision" -> "c4.mod.domain.c4perscar",
  "c4.mod.domain.decision" -> "c4.mod.domain.c4roadunit",

  "c4.cargo.decision" -> "c4.cargo.c4gencargo",


  //"c4.mod.domain.c4placement" -> "c4.mod.domain.c4perscar",   //temporary
  "c4.cargo.base" -> "c4.mod.domain.c4cnt.base",              //temporary
  "c4.cargo.base" -> "c4.mod.domain.c4gencargo",              //temporary
  "c4.cargo.base" -> "c4.mod.domain.c4roadunit",              //temporary
  "c4.cargo.base" -> "c4.mod.c4statemachine.service_order.base",
  "c4.cargo.base" -> "c4.mod.vehicle.base",
  //"c4.mod.domain.decision" -> "c4.cargo.decision",
)

private lazy val srcArchi = Archi(Process("git show HEAD:uses.archimate", new File("../c4enterprise/")).!!)
private lazy val dstArchi = Archi(Files.readString(archiDst))


def findPrintModules(nameSubstring: String): Unit =
  dstArchi.byId.values
    .map(_.name)
    .filter(_.contains(nameSubstring))
    .toList
    .sorted
    .foreach(println)


def printModuleProjects(modules: Node*): Unit =
  for module <- modules yield
    println(s"\n${module.name}:")
    module.targetProjects.toList.map("  " + _.name).sorted.foreach(println)


def printModuleProjectDiff(module1: Node, module2: Node): Unit =
  val List(projects1, projects2) = List(module1, module2).map(_.targetProjects)
  def print(module: Node, selfProjects: Set[Node], otherProjects: Set[Node]): Unit =
    println(s"Only in ${module.name}:")
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


def rewrite(): Unit =
  srcArchi match { case archi =>
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


@main def main(): Unit =
  import dstArchi.nameToNode

  //printModuleProjectDiff("c4.mod.domain.c4cnt.base", "c4.cargo.c4container.base")
  //dstArchi.getPath("c4.mod.domain.c4techflow", "c4.cargo.c4container.base").foreach(println)
  //printModuleProjects("c4.cargo.c4perscar")
  //findPrintModules("service")
  rewrite();  printProjectsDiff(toAddModules.toArray*)
  //printInvalid()
