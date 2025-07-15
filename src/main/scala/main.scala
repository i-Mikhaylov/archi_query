import java.nio.file.{Files, Paths}


private val archiSrc = "renamed.archimate"
private val archiDst = "../c4enterprise/uses.archimate"

private val toRemoveDependencies = List[(String, String)](
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4roadunit",
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4cnt.base",
  "c4.cargo.decision" -> "c4.mod.domain.c4roadunit",
  "c4.cargo.decision" -> "c4.mod.domain.c4cnt.base",
  "c4.cargo.imo" -> "c4.mod.domain.c4cnt.base",


  //toMain
  //"c4.cargo.operationalparameters" -> "c4.cargo.base",        to the end

  //mainToDomain
  //"c4.mod.domain.c4placement" -> "c4.mod.domain.c4roadunit",  to the end
  //"c4.mod.c4srl.base" -> "c4.mod.domain.tug",                 to the end
  "c4.cargo.decision" -> "c4.mod.domain.c4perscar",
  "c4.mod.c4srl.base" -> "c4.mod.wtms.c4vessel.base",
  "c4.mod.domain.c4placement" -> "c4.mod.domain.c4perscar",
  //"c4.mod.c4srl.base" -> "c4.mod.wtms.c4bulkcargo",
  //"c4.mod.domain.c4placement" -> "c4.mod.domain.c4cnt.base",
  //"c4.cargo.decision" -> "c4.mod.domain.c4maficargo",
  //"c4.cargo.base" -> "c4.mod.domain.tug",
  //"c4.cargo.decision" -> "c4.mod.domain.tug",
  //"c4.cargo.decision" -> "c4.mod.domain.c4gencargo",
  //"c4.cargo.base" -> "c4.mod.domain.c4maficargo",
  //"c4.mod.c4srl.base" -> "c4.mod.domain.c4maficargo",
  //"c4.mod.domain.c4placement" -> "c4.mod.domain.c4gencargo",
  //"c4.mod.domain.c4placement" -> "c4.mod.domain.rwcar_base",

  //mainToCargo
  //"c4.mod.c4srl.base" -> "c4.cargo.c4gencargo",
  //"c4.mod.c4srl.base" -> "c4.cargo.oogresolution",
  //"c4.mod.c4srl.base" -> "c4.cargo.imoresolution.base",
  //"c4.mod.c4srl.base" -> "c4.cargo.imoresolution.base",

  //modToDomain
  //"c4.cargo.inspection" -> "c4.mod.wtms.c4bulkcargo",

  //modCargoToCargo
  //"c4.cargo.c4container.base" -> "c4.cargo.c4container.job",
  //"c4.cargo.imoresolution.base" -> "c4.cargo.imoresolution.withdrawpolicyany",
  //"c4.cargo.c4roadunit.base" -> "c4.cargo.c4roadunit.megacanvas",
  //"c4.cargo.c4container.base" -> "c4.cargo.tiers_in_bundle",
  //"c4.cargo.c4container.base" -> "c4.cargo.c4container.allocrule",
  //"c4.cargo.imoresolution.base" -> "c4.cargo.imoresolution.notification",
  //"c4.cargo.imo" -> "c4.cargo.imoresolution.base",

)
private val toAddDependencies = List[(String, String)](
  "c4.mod.domain.c4techflow" -> "c4.mod.domain.c4cnt.base",
  "c4.cargo.base" -> "c4.mod.domain.c4perscar",
)


private lazy val srcArchi = ArchiRich(Archi(Files.readString(Paths.get(archiSrc))))
private lazy val dstArchi = ArchiRich(Archi(Files.readString(Paths.get(archiDst))))


def findPrintModules(nameSubstring: String): Unit =
  dstArchi.archi.nodes.values
    .map(_.name)
    .filter(_.contains(nameSubstring))
    .toList
    .sorted
    .foreach(println)


def printModuleProjects(moduleNames: String*): Unit =
  moduleNames.foreach { moduleName =>
    val getPath = Node.pathToSource(srcArchi.nodeByName(moduleName).id)
    println()
    srcArchi.projects
      .filter(getPath(_).nonEmpty)
      .map(_.name)
      .sorted
      .foreach(println)
  }


def printInvalid(): Unit =
  val nodes = dstArchi.archi.nodes.values
  def byName(names: String*) = names.map(dstArchi.nodeByName)

  val mainDomain = byName("c4.core.inheritance", "c4.mod.domain.cargo")
  val mainCargo = byName("c4.mod.domain.c4placement", "c4.cargo.base", "c4.cargo.decision")
  val mainSrl = byName("c4.mod.c4srl.base", "c4.mod.wtms.c4cargo")
  val mainCargoSrl = (mainCargo ++ mainSrl).toSet
  val allMain = mainCargoSrl ++ mainDomain

  val modDomain = byName(
    "c4.mod.domain.breakbulkcargo",
    "c4.mod.domain.c4cnt.base",
    "c4.mod.domain.c4gencargo",
    "c4.mod.domain.c4maficargo",
    "c4.mod.domain.c4perscar",
    "c4.mod.domain.c4roadunit",
    "c4.mod.domain.rwcar_base",
    "c4.mod.domain.tug",
    "c4.mod.wtms.c4bulkcargo",
    "c4.mod.wtms.c4vessel.base",

    "c4.mod.domain.railway",
  ).toSet
  val modCargo = nodes.filter(node => node.name.startsWith("c4.cargo") && !modDomain.contains(node) && !allMain.contains(node))
  val mod = modDomain ++ modCargo

  def targetFilter(src: Iterable[Node], filter: Node => Boolean) = for {
    node <- src.view
    target <- node.targets
    if filter(target)
  } yield node -> target
  def sourceFilter(src: Iterable[Node], filter: Node => Boolean) = for {
    node <- src
    source <- node.sources
    if filter(source)
  } yield source -> node

  val toMain = targetFilter(mod, allMain.contains)
  val mainToDomain = sourceFilter(modDomain, mainCargoSrl.contains)
  val mainToCargo = sourceFilter(modCargo, mainSrl.contains)
  val modToDomain = sourceFilter(modDomain, mod.contains)
  val modCargoToCargo = sourceFilter(modCargo, modCargo.toSet.contains)

  def print(name: String, dependencies: Iterable[(Node, Node)]): Unit =
    println(s"\n$name:")
    dependencies.foreach((source, target) => println(source.name + " -> " + target.name))
  print("toMain",           toMain)
  print("mainToDomain",     mainToDomain)
  print("mainToCargo",      mainToCargo)
  print("modToDomain",      modToDomain)
  print("modCargoToCargo",  modCargoToCargo)


def rewrite(): Unit =
  val byName = srcArchi.nodeByName

  val toRemoveIds = toRemoveDependencies
    .map((fromName, toName) => byName(fromName).id -> byName(toName).id)
  val toAddIds = toAddDependencies
    .map((fromName, toName) => byName(fromName).id -> byName(toName).id)
  val updated = srcArchi.archi.removeDependencies(toRemoveIds.toSet).addDependencies(toAddIds)
  Files.writeString(Paths.get(archiDst), updated.toString)


@main def main(): Unit =

  //println(srcArchi.moduleProjectDiff("c4.mod.c4srl.base", "c4.mod.domain.cargo"))
  //printModuleProjects("c4.mod.domain.tug")
  //findPrintModules("techflow")
  //rewrite()
  printInvalid()

