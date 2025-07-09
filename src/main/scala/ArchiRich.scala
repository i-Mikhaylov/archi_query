

case class ArchiRich(archi: Archi):
  lazy val nodeByName: Map[String, Node] = archi.nodes.values.map(node => node.name -> node).toMap
  lazy val projects: List[Node] = archi.nodes.values.filter(_.nodeType == Node.Project).toList

  type NodePath = List[Node]
  def getModuleProjectPaths(moduleName: String): List[NodePath] =
    val getPath = Node.pathToChild(nodeByName(moduleName).id)
    projects.map(getPath).filter(_.nonEmpty)

  def moduleProjectDiff(moduleName1: String, moduleName2: String): (List[NodePath], List[NodePath]) =
    def pathsAndProjects(moduleName: String) =
      val pathList = getModuleProjectPaths(moduleName)
      val projectSet = pathList.map(_.head.id).toSet
      pathList -> projectSet
    def filter(paths: List[NodePath], projects: Set[String]) =
      paths.filterNot(path => projects.contains(path.head.id))
    val (paths1, projects1) = pathsAndProjects(moduleName1)
    val (paths2, projects2) = pathsAndProjects(moduleName2)
    filter(paths1, projects2) -> filter(paths2, projects1)
