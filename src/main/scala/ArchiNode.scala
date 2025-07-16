import scala.collection.View

/*
object Node:
  private def pathTo(getNext: Node => List[Node], keyId: String, transform: List[Node] => List[Node]) =
    lazy val memo: Node => List[Node] = Memo.mutableHashMapMemo { node =>
      if (node.id == keyId) node :: Nil
      else getNext(node).view
        .map(memo)
        .find(_.nonEmpty)
        .map(node :: _)
        .getOrElse(Nil)
    }
    memo andThen transform andThen NodePath.apply
  def pathToSource(sourceId: String): Node => NodePath = pathTo(_.sources, sourceId, _.reverse)
  def pathToTarget(targetId: String): Node => NodePath = pathTo(_.targets, targetId, identity)
*/

case class Node(id: String, name: String, isProject: Boolean)(_sources: => List[Node], _targets: => List[Node]):
  def sources: List[Node] = _sources
  def targets: List[Node] = _targets
  override def hashCode: Int = id.hashCode
  override def equals(obj: Any): Boolean = obj match
    case Node(otherId, _, _) => otherId == id
    case _ => false

  protected def allSources: View[Node] = sources.view.flatMap(child => child.allSources ++ Some(child))
  def graphvizSources: String =
    val lines = for {
      node <- (this.allSources ++ Some(this)).toSet.toList
      source <- node.sources
    } yield s"\t\"${source.name}\" -> \"${node.name}\""
    "digraph G {\n" + lines.mkString("\n") + "\n}"


case class NodePath(nodes: List[Node]):
  val size: Int = nodes.size
  override def toString: String =
    if (nodes.nonEmpty) nodes.map(_.name).mkString(" -> ")
    else "No path"


implicit val nodeOrdering: Ordering[Node] = Ordering.by(_.name)
implicit val nodePathOrdering: Ordering[NodePath] =
  Ordering.by((_: NodePath).size) orElse Ordering.by((_: NodePath).nodes)(implicitly)
