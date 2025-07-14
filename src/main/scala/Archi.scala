import Printer.*
import scalaz.Memo

import scala.collection.View
import scala.util.{Random, Try}
import scala.xml.{Attribute, Elem, NodeSeq, Null, Text, XML, Node as XmlNode}


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

  def update(
    dependency: Option[XmlNode] = None,
    node: Option[XmlNode] = None,
    diagram: Option[XmlNode] = None,
  ): Elem =
    val updateMap = {
      dependency.map("relations" -> _).toList :::
      node.map("application" -> _).toList :::
      diagram.map("diagrams" -> _).toList
    }.toMap
    xml updateChildren xml.child.map { folder =>
      Option.when(folder.label == "folder")(folder.singleGetAttr("type"))
        .flatMap(updateMap.get)
        .getOrElse(folder)
    }


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
  def pathToSource(childId: String): Node => List[Node] = pathTo(_.sources, childId)
  def pathToTarget(childId: String): Node => List[Node] = pathTo(_.targets, childId)

case class Node(id: String, name: String, nodeType: Node.Type)(_sources: => List[Node], _targets: => List[Node]):
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

case class Dependency(id: String, from: Node, to: Node)

implicit class BeautifulNodePath(value: List[Node]):
  def beautifulString: String = value.map(_.name).mkString(" <- ")


class Archi private(xml: Elem, fileBegin: String):

  lazy val nodes: Map[String, Node] =
    def unknowXsiTypeWarning(xsiType: String): Unit = printWarn(s"Unknown application element xsi:type: $xsiType")
    val folders: Folders = Folders(xml)

    val dependencies = (folders.dependency \ "element")
      .filter { element =>
        val xsiType = element.singleGetAttr("xsi:type")
        val typeValid = xsiType == "archimate:ServingRelationship"
        if (!typeValid) unknowXsiTypeWarning(xsiType)
        typeValid
      }
      .map { element => element.singleGetAttr("source") -> element.singleGetAttr("target") }
    val sourceNodeIds = dependencies.groupMap(_._2)(_._1)
    val targetNodeIds = dependencies.groupMap(_._1)(_._2)

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
        lazy val sources = sourceNodeIds.getOrElse(id, Nil).map(nodes).toList
        lazy val targets = targetNodeIds.getOrElse(id, Nil).map(nodes).toList
        id -> Node(id, name, elementType)(sources, targets)
      }
      .toMap
      .withDefault(key => throw Exception(s"Node with key $key element not found"))


  type RemoveSC = (Seq[XmlNode], Seq[String])

  private def removeSCInChild(child: XmlNode, relationIds: Set[String]): RemoveSC = child.child.map { grandChild =>
    val isSourceConnection = grandChild.label == "sourceConnection"
    if (isSourceConnection && grandChild.singleGetAttr("xsi:type") != "archimate:Connection")
      throw Exception(s"Found unknow xsi:type: $grandChild")
    lazy val targetRelation = relationIds contains grandChild.singleGetAttr("archimateRelationship")
    if (isSourceConnection && targetRelation) Right(grandChild.singleGetAttr("id"))
    else Left(grandChild)
  }.partitionMap(identity)

  private def removeSCInElement(element: XmlNode, relationIds: Set[String]): RemoveSC = element.child.flatMap { child =>
    lazy val (grandChildren, sourceConnectionIds) = child.singleGetAttr("xsi:type") match {
      case "archimate:DiagramObject" => removeSCInChild(child, relationIds)
      case "archimate:Group" => removeSCInElement(child, relationIds)
      case _ => (child.child, Nil)
    }
    if (child.label != "child" || sourceConnectionIds.isEmpty) Left(child) :: Nil
    else Left(child.updateChildren(grandChildren)) :: sourceConnectionIds.map(Right(_)).toList
  }.partitionMap(identity)

  private def removeSCInFolder(folder: XmlNode, relationIds: Set[String]): RemoveSC = folder.child.flatMap { element =>
    lazy val (children, sourceConnectionIds) = removeSCInElement(element, relationIds)
    val isElement = element.label == "element"
    if (isElement && element.singleGetAttr("xsi:type") != "archimate:ArchimateDiagramModel")
      throw Exception(s"Found unknown xsi:type: ${element.toString.substring(0, 10000 min element.length)}")
    if (!isElement || sourceConnectionIds.isEmpty) Left(element) :: Nil
    else Left(element.updateChildren(children)) :: sourceConnectionIds.map(Right(_)).toList
  }.partitionMap(identity)

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

    val (diagramElements1, sourceConnectionIdSeq) = removeSCInFolder(folders.diagram, relationIds)
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

    val updatedXml = folders.update(dependency = Some(dependencyFolderUpdated), diagram = Some(diagramFolderUpdated))
    new Archi(updatedXml, fileBegin)

  def addDependencies(dependencies: Iterable[(String, String)]): Archi =
    val folders: Folders = Folders(xml)

    val existingIds = (folders.dependency \ "element")
      .map(_.singleGetAttr("id"))
      .flatMap { id => Try(Integer.parseUnsignedInt(id, 16)).toOption }
      .toSet
    def generateId = LazyList.continually(Archi.random.nextInt)
      .dropWhile(existingIds.contains)
      .map(id => f"$id%08x")
      .head

    val texts = folders.dependency.child.reverseIterator.filter(_.isInstanceOf[Text])
    val lastNewLine = texts.next
    val newLine = texts.next

    val updatedChildren = folders.dependency.child.drop(1) ++ dependencies.flatMap { case (from, to) =>
      newLine ::
      <element xsi:type="archimate:ServingRelationship" id={generateId} source={from} target={to}/> ::
      Nil
    } ++ lastNewLine
    val updatedDependencyFolder = folders.dependency.updateChildren(updatedChildren)
    val updatedXml = folders.update(dependency = Some(updatedDependencyFolder))
    new Archi(updatedXml, fileBegin)

  override def toString: String =
    val orig = xml.toString
    val fixedBegin = fileBegin + orig.substring(orig.indexOf('\n')) + '\n'
    val noEmptyLines = fixedBegin.replaceAll("\\n(\\s*\\n)+", "\n")
    noEmptyLines


object Archi:
  private val random = new Random(123456789123456789L)

  def apply(xml: String): Archi =
    val fileBegin = xml.substring(0, xml.indexOf('\n', xml.indexOf('\n') + 1))
    new Archi(XML.loadString(xml), fileBegin)
