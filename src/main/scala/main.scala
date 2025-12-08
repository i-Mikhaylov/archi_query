package processor

import archi.{Archi, Node}

import scala.language.implicitConversions


def findModule(substrings: Iterable[String])(implicit archi: Archi): Unit =
  archi.byId.values
    .map(_.name)
    .filter(name => substrings.exists(name.contains))
    .toList
    .sorted
    .foreach(println)


def moduleProjects(modules: Iterable[Node]): Unit =
  def processModule(module: Node, prefix: String): Unit =
    module.targetProjects.toList.map(prefix + _.name).sorted.foreach(println)
  if modules.sizeCompare(1) == 0 then
    processModule(modules.head, "")
  else
      for module <- modules yield
        println(s"\n${module.name}:")
        processModule(module, "  ")
        println()


def moduleDiff(module1: Node, module2: Node): Unit =
  val List(projects1, projects2) = List(module1, module2).map(_.targetProjects)
  def print(module: Node, selfProjects: Set[Node], otherProjects: Set[Node]): Unit =
    println(s"Only ${module.name}:")
    selfProjects.view.filterNot(otherProjects.contains).map("  " + _.name).toList.sorted.foreach(println)
  print(module1, projects1, projects2)
  print(module2, projects2, projects1)


def projectDiff(srcArchi: Archi, dstArchi: Archi, ignoreModuleNames: Set[String] = Set.empty): Unit =
  for projectId <- (srcArchi.projects ::: dstArchi.projects).distinct.sortBy(_.name).map(_.id) do

    val List((srcProject, srcModules), (dstProject, dstModules)) = List(srcArchi, dstArchi)
      .map(_.byId.get(projectId).filter(_.isProject))
      .map { project => project -> project.map(_.allSources).getOrElse(Set.empty) }

    val onlySrcModules = srcModules.view.filter(n => !dstModules.contains(n) && !ignoreModuleNames.contains(n.name)).toList
    val onlyDstModules = dstModules.view.filter(n => !srcModules.contains(n) && !ignoreModuleNames.contains(n.name)).toList
    if (onlySrcModules.nonEmpty || onlyDstModules.nonEmpty)
      println((dstProject orElse srcProject).get.name + ":")
      if (onlySrcModules.nonEmpty) println("  - " + onlySrcModules.map(_.name).sorted.mkString("\n    "))
      if (onlyDstModules.nonEmpty) println("  + " + onlyDstModules.map(_.name).sorted.mkString("\n    "))
