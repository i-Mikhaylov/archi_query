import scala.xml.{Attribute, Elem, NodeSeq, Null, PrefixedAttribute, XML, Node as XmlNode}
import Printer.*
import scalaz.Memo

import scala.collection.View
import scala.util.Try


private implicit class SingleElementSeq[T](seq: Seq[T]):
  def single(name: String): T = {
    def truncatedName = if (name.length > 10000) name.substring(0, 10000) + "..." else name
    seq match {
      case Seq() => throw Exception(s"Not found $truncatedName")
      case Seq(single) => single
      case _ => throw Exception(s"Found multiple occurrences of $truncatedName")
    }
  }

private implicit class RichXmlNode[T](node: XmlNode):
  def singleGet(query: String): XmlNode = (node \ query).single(s"$query (${node.toString})")

  def singleGetAttr(query: String): String = node.attributes
    .filter(_.prefixedKey == query)
    .flatMap(_.value)
    .toList
    .single(s"attr $query (${node.toString})")
    .text

  def mapAttrs(op: (String, String) => Option[String]): Elem = node match {
    case elem: Elem =>
      val attributes = elem.attributes.reduceRight {
        case (attr: Attribute, acc) =>
          op(attr.prefixedKey, attr.value.text).fold(acc) { Attribute(attr.pre, attr.key, _, acc) }
        case other => Null
      }
      elem.copy(attributes = attributes)
    case other => throw Exception(s"Can't map attributes on non Elem: $other")
  }

  def updateChildren(children: Seq[XmlNode]): Elem = node match {
    case elem: Elem => elem.copy(child = children)
    case other => throw Exception(s"Can't update children on non Elem: $other")
  }

private class Folders(xml: Elem):
  private val all = xml \ "folder"

  def apply(folderType: String): XmlNode =
    all.filter(_.singleGetAttr("type") == folderType).single(folderType.capitalize + " Folder")

  lazy val dependency: XmlNode = apply("relations")
  lazy val node: XmlNode = apply("application")
  lazy val diagram: XmlNode = apply("diagrams")


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

case class Dependency(id: String, from: Node, to: Node)

implicit class NodePath(value: List[Node]):
  def beautifulString: String = value.map(_.name).mkString(" <- ")


class Archi private(xml: Elem, fileBegin: String):
  
  lazy val nodes: Map[String, Node] =
    def unknowXsiTypeWarning(xsiType: String): Unit = printWarn(s"Unknown application element xsi:type: $xsiType")
    val folders: Folders = Folders(xml)

    val dependencyElements = (folders.dependency \ "element").filter { element =>
      val xsiType = element.singleGetAttr("xsi:type")
      val typeValid = xsiType == "archimate:ServingRelationship"
      if (!typeValid) unknowXsiTypeWarning(xsiType)
      typeValid
    }
    val idByDependency = dependencyElements.map { element =>
      (element.singleGetAttr("source"), element.singleGetAttr("target")) -> element.singleGetAttr("id")
    }.toMap
    val childNodeIds = idByDependency.keys.groupMap(_._2)(_._1)
    val parentNodeIds = idByDependency.keys.groupMap(_._1)(_._2)

    (folders.node \ "element")
      .flatMap { element =>
        element.singleGetAttr("xsi:type") match {
          case "archimate:ApplicationFunction" => Some(element -> Node.Module)
          case "archimate:ApplicationComponent" => Some(element -> Node.Project)
          case unknownType => unknowXsiTypeWarning(unknownType); None
        }
      }
      .map { case (element, elementType) =>
        val id = element.singleGetAttr("id")
        val name = element.singleGetAttr("name")
        lazy val children = childNodeIds.getOrElse(id, Nil).map(nodes).toList
        lazy val parents = parentNodeIds.getOrElse(id, Nil).map(nodes).toList
        id -> Node(id, name, elementType)(children, parents)
      }
      .toMap
      .withDefault(key => throw Exception(s"Node with key $key element not found"))


  def removeDependencies(dependencies: Set[(String, String)]): Archi =
    val folders: Folders = Folders(xml)

    val (dependencyElements, relationIdSeq) = folders.dependency.child.partitionMap { element =>
      val isTarget = element.label == "element" &&
        dependencies.contains(element.singleGetAttr("source"), element.singleGetAttr("target"))
      if (isTarget) Right(element.singleGetAttr("id"))
      else Left(element)
    }
    val dependencyFolderUpdated = folders.dependency.updateChildren(dependencyElements)
    val relationIds = relationIdSeq.toSet

    val (diagramElements1, sourceConnectionIdSeq) = folders.diagram.child.flatMap { element =>
      val isElement = element.label == "element"
      if (isElement && element.singleGetAttr("xsi:type") != "archimate:ArchimateDiagramModel")
        throw Exception(s"Found unknown xsi:type: ${element.toString.substring(0, 10000 min element.length)}")

      lazy val (children, sourceConnectionIds) = element.child.flatMap { child =>
        lazy val (grandChildren, sourceConnectionIds) = child.child.map { grandChild =>
          val isSourceConnection = grandChild.label == "sourceConnection"
          lazy val targetRelation = relationIds contains grandChild.singleGetAttr("archimateRelationship")
          if (isSourceConnection && grandChild.singleGetAttr("xsi:type") != "archimate:Connection")
            throw Exception(s"Found unknow xsi:type: $grandChild")
          if (isSourceConnection && targetRelation) Right(grandChild.singleGetAttr("id"))
          else Left(grandChild)
        }.partitionMap(identity)

        val isTargetChild = child.label == "child" && child.singleGetAttr("xsi:type") == "archimate:DiagramObject"
        if (!isTargetChild || sourceConnectionIds.isEmpty) Left(child) :: Nil
        else Left(child.updateChildren(grandChildren)) :: sourceConnectionIds.map(Right(_)).toList
      }.partitionMap(identity)

      if (!isElement || sourceConnectionIds.isEmpty) Left(element) :: Nil
      else Left(element.updateChildren(children)) :: sourceConnectionIds.map(Right(_)).toList
    }.partitionMap(identity)
    val sourceConnectionIds = sourceConnectionIdSeq.toSet

    val diagramElements2 = diagramElements1.map { element =>
      lazy val children = element.child.map { child =>
        if (child.label != "child") child
        else child.mapAttrs {
          case ("targetConnections", idStr) => Some(idStr.split(' ').filterNot(sourceConnectionIds.contains).mkString(" "))
          case (otherKey, value) => Some(value)
        }
      }
      if (element.label == "element") element.updateChildren(children)
      else element
    }
    val diagramFolderUpdated = folders.diagram.updateChildren(diagramElements2)

    val foldersUpdated = xml.child.map { folder =>
      if (folder.label != "folder") folder
      else folder.singleGetAttr("type") match {
        case "relations" => dependencyFolderUpdated
        case "diagrams" => diagramFolderUpdated
        case _ => folder
      }
    }
    new Archi(xml.updateChildren(foldersUpdated), fileBegin)

  
  override def toString: String =
    val text = xml.toString
    fileBegin + text.substring(text.indexOf('\n')) + '\n'
    
    
object Archi:
  def apply(xml: String): Archi =
    val fileBegin = xml.substring(0, xml.indexOf('\n', xml.indexOf('\n') + 1))
    new Archi(XML.loadString(xml), fileBegin)
