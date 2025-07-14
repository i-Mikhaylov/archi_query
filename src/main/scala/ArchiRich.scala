

type NodePath = List[Node]

case class ModuleProjectDiff(diff1: List[NodePath], diff2: List[NodePath], name1: String, name2: String):
  override def toString: String =
    List(name1 -> diff1, name2 -> diff2)
      .map { case (name, diff) => name + ": " + diff.map(_.head.name).mkString(", ") }
      .mkString("\n")

case class ArchiRich(archi: Archi):
  lazy val nodeByName: Map[String, Node] = archi.nodes.values.map(node => node.name -> node).toMap
  lazy val projects: List[Node] = archi.nodes.values.filter(_.isProject).toList

  def getModuleProjectPaths(moduleName: String): List[NodePath] =
    val getPath = Node.pathToSource(nodeByName(moduleName).id)
    projects.map(getPath).filter(_.nonEmpty)

  def moduleProjectDiff(moduleName1: String, moduleName2: String): ModuleProjectDiff =
    def pathsAndProjects(moduleName: String) =
      val pathList = getModuleProjectPaths(moduleName)
      val projectSet = pathList.map(_.head.id).toSet
      pathList -> projectSet
    def filter(paths: List[NodePath], projects: Set[String]) =
      paths.filterNot(path => projects.contains(path.head.id))
    val (paths1, projects1) = pathsAndProjects(moduleName1)
    val (paths2, projects2) = pathsAndProjects(moduleName2)
    val diff1 = filter(paths1, projects2)
    val diff2 = filter(paths2, projects1)
    ModuleProjectDiff(diff1, diff2, moduleName1, moduleName2)
