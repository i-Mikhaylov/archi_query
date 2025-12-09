package processor

import archi.{Archi, Node}
import util.ListIf

import scala.language.implicitConversions


def findModule(substrings: Iterable[String])(implicit archi: Archi): List[String] =
  archi.allModules
    .map(_.name)
    .filter(name => substrings.exists(name.contains))
    .toList
    .sorted


def moduleProjects(module: Node): List[String] =
  s"\n${module.name}:" ::
    module.targetProjects.toList.map("  " + _.name).sorted


def moduleProjectDiff(module1: Node, module2: Node): List[String] =
  def process(module: Node, selfProjects: Set[Node], otherProjects: Set[Node]) =
    s"Only ${module.name}:" ::
      selfProjects.view.filterNot(otherProjects.contains).map("  " + _.name).toList.sorted
  val projects1 = module1.targetProjects
  val projects2 = module2.targetProjects
  process(module1, projects1, projects2) :::
  process(module2, projects2, projects1)


def archiDiff(srcArchi: Archi, dstArchi: Archi, ignoreModuleNames: Set[String] = Set.empty): List[String] =

  def process(projectId: String) =
    val List((srcProject, srcModules), (dstProject, dstModules)) = List(srcArchi, dstArchi)
      .map(_.byId.get(projectId).filter(_.isProject))
      .map { project => project -> project.map(_.allSources).getOrElse(Set.empty) }

    val onlySrcModules = srcModules.view.filter(n => !dstModules.contains(n) && !ignoreModuleNames.contains(n.name)).toList
    val onlyDstModules = dstModules.view.filter(n => !srcModules.contains(n) && !ignoreModuleNames.contains(n.name)).toList
    if (onlySrcModules.nonEmpty || onlyDstModules.nonEmpty)
      ((dstProject orElse srcProject).get.name + ":") ::
      ListIf(onlySrcModules.nonEmpty) { "  - " + onlySrcModules.map(_.name).sorted.mkString("\n    ") } :::
      ListIf(onlyDstModules.nonEmpty) { "  + " + onlyDstModules.map(_.name).sorted.mkString("\n    ") }
    else Nil

  (srcArchi.projects ::: dstArchi.projects)
    .distinct
    .sortBy(_.name)
    .flatMap(node => process(node.id))
