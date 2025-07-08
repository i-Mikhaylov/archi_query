import scala.xml.{XML, Node as XmlNode}
import Printer.*
import scalaz.Memo

import scala.collection.View


object Node:
  sealed trait Type
  object Module extends Type
  object Project extends Type

  private def pathTo(getTarget: Node => List[Node], targetId: String): Node => List[Node] =
    lazy val memo: Node => List[Node] = Memo.mutableHashMapMemo { node =>
      if (node.id == targetId) node :: Nil
      else getTarget(node).view
        .map(memo)
        .find(_.nonEmpty)
        .map(node :: _)
        .getOrElse(Nil)
    }
    memo
  def pathToChild(childId: String): Node => List[Node] = pathTo(_.children, childId)
  def pathToParent(childId: String): Node => List[Node] = pathTo(_.parents, childId)


case class Node(id: String, name: String, nodeType: Node.Type)(_children: => List[Node], _parents: => List[Node]):
  def children: List[Node] = _children
  def parents: List[Node] = _parents
  override def hashCode: Int = id.hashCode
  override def equals(obj: Any): Boolean = obj match
    case Node(otherId, _, _) => otherId == id
    case _ => false

  protected def allChildren: View[Node] = children.view.flatMap(child => child.allChildren ++ Some(child))
  def graphvizChildren = {
    val lines = for {
      parent <- (this.allChildren ++ Some(this)).toSet.toList
      child <- parent.children
    } yield s"\t\"${child.name}\" -> \"${parent.name}\""
    "digraph G {\n" + lines.mkString("\n") + "\n}"
  }


case class Archi(nodes: Map[String, Node]) {}


object Archi:

  private case class Dependency(id: String, from: String, to: String)
  case class NodePath(value: List[Node]) {
    override def toString: String = value.map(_.name).mkString(" <- ")
  }

  private implicit class SingleElementSeq[T](seq: Seq[T]):
    def single(name: String): T = seq match {
      case Seq() => throw Exception(s"$name not found")
      case Seq(single) => single
      case _ => throw Exception(s"Found multiple occurrences of $name")
    }

  private implicit class XmlSingleGet[T](node: XmlNode):
    def singleGet(query: String): XmlNode = (node \ query).single(s"$query (${node.toString})")
    def singleGetAttr(query: String): String = node.attributes
      .filter(_.prefixedKey == query)
      .flatMap(_.value)
      .toList
      .single(s"$query (${node.toString})")
      .text




  private def unknowXsiTypeWarning(xsiType: String): Unit =
    printWarn(s"Unknown application element xsi:type: $xsiType")

  def apply(xml: String): Archi =

    val root = XML.loadString(xml)
    val folders = root \ "folder"


    val relationFolder = folders.filter(_.singleGetAttr("type") == "relations").single("Relation Folder")
    val dependencyById = (relationFolder \ "element")
      .filter { element =>
        val xsiType = element.singleGetAttr("xsi:type")
        val typeValid = xsiType == "archimate:ServingRelationship"
        if (!typeValid) unknowXsiTypeWarning(xsiType)
        typeValid
      }
      .map { element =>
        val id = element.singleGet("@id").text
        val from = element.singleGet("@source").text
        val to = element.singleGet("@target").text
        id -> Dependency(id, from, to)
      }
      .toMap

    val childNodeIds = dependencyById.values.groupMap(_.to)(_.from)
    val parentNodeIds = dependencyById.values.groupMap(_.from)(_.to)

    val applicationFolder = folders.filter(_.singleGetAttr("type") == "application").single("Application Folder")
    lazy val elementById: Map[String, Node] = (applicationFolder \ "element")
      .flatMap { element =>
        element.singleGetAttr("xsi:type") match {
          case "archimate:ApplicationFunction" => Some(element -> Node.Module)
          case "archimate:ApplicationComponent" => Some(element -> Node.Project)
          case unknownType => unknowXsiTypeWarning(unknownType); None
        }
      }
      .map { case (element, elementType) =>
        val id = element.singleGet("@id").text
        val name = element.singleGet("@name").text
        lazy val children = childNodeIds.getOrElse(id, Nil).map(elementById).toList
        lazy val parents = parentNodeIds.getOrElse(id, Nil).map(elementById).toList
        id -> Node(id, name, elementType)(children, parents)
      }
      .toMap
      .withDefault(key => throw Exception(s"Node with key $key element not found"))


    Archi(elementById)
