package archi



case class Node(id: String, name: String, isProject: Boolean)(_sources: => List[Node], _targets: => List[Node]):
  override def hashCode: Int = id.hashCode
  override def equals(obj: Any): Boolean = obj match
    case node: Node => node.id == id
    case _ => false

  def sources: List[Node] = _sources
  def targets: List[Node] = _targets

  lazy val targetProjects: Set[Node] = targets.view
    .flatMap(target => if (target.targets.isEmpty) Some(target).filter(_.isProject) else target.targetProjects)
    .toSet

  lazy val allSources: Set[Node] = sources.view.flatMap(source => source.allSources + source).toSet
  def graphvizSources: String =
    val lines = for {
      node <- (this.allSources + this).toList
      source <- node.sources
    } yield s"\t\"${source.name}\" -> \"${node.name}\""
    "digraph G {\n" + lines.mkString("\n") + "\n}"


case class NodePath(nodes: List[Node], count: Int):
  val size: Int = nodes.size
  override def toString: String =
    if (nodes.nonEmpty) nodes.map(_.name).mkString(" -> ") + (if (count > 1) s" (x$count)" else "")
    else "No path"


implicit val nodeOrdering: Ordering[Node] = Ordering.by(_.name)
implicit val nodePathOrdering: Ordering[NodePath] =
  Ordering.by((_: NodePath).size) orElse Ordering.by((_: NodePath).nodes)(implicitly)
