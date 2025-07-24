import Printer.*

import scala.annotation.tailrec
import scala.language.implicitConversions
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


class Archi private(
  xml: Elem,
  fileBegin: String,
  random: Random = new Random(123456789123456789L),
):

  lazy val byId: Map[String, Node] =
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
          case "archimate:ApplicationFunction" => Some(element -> false)
          case "archimate:ApplicationComponent" => Some(element -> true)
          case unknownType => unknowXsiTypeWarning(unknownType); None
        }
      }
      .map { case (element, isProject) =>
        val id = element.singleGetAttr("id")
        val name = element.singleGetAttr("name")
        lazy val sources = sourceNodeIds.getOrElse(id, Nil).map(byId).toList
        lazy val targets = targetNodeIds.getOrElse(id, Nil).map(byId).toList
        id -> Node(id, name, isProject)(sources, targets)
      }
      .toMap
      .withDefault(key => throw Exception(s"Node with key $key element not found"))

  lazy val byName: Map[String, Node] = byId.values.map(node => node.name -> node).toMap

  lazy val projects: List[Node] = byId.values.filter(_.isProject).toList

  implicit def nameToNode(name: String): Node = byName(name)

  @tailrec private def getPathInner(key: Node, halfPaths: List[List[Node]], fullPaths: List[List[Node]]): List[List[Node]] =
    val nextPaths =
      for
        halfPath <- halfPaths
        source <- halfPath.head.sources
        if source.allSources.contains(key) || source == key
      yield source :: halfPath
    val (nextHalf, nextFull) = nextPaths.partition(_.head != key)
    val allFull = nextFull ::: fullPaths
    if (nextHalf.isEmpty) allFull
    else getPathInner(key, nextHalf, allFull)
  def getPath(from: Node, to: Node): List[NodePath] =
    getPathInner(from, List(to :: Nil), Nil).map(NodePath(_)).sorted


  private def getIds(elem: Elem): Seq[String] =
    (elem \ "@id").map(_.text) ++ elem.child.flatMap {
      case elem: Elem => getIds(elem)
      case _ => Nil
    }
  private lazy val existingIds: Set[Int] = getIds(xml)
    .flatMap { id => Try(Integer.parseUnsignedInt(id, 16)).toOption }
    .toSet
  private def generateId =
    LazyList.continually(random.nextInt)
      .dropWhile(existingIds.contains)
      .map(id => f"$id%08x")
      .head

  def addModules(names: Iterable[String]): Archi =
    val folders: Folders = Folders(xml)
    val updatedChildren = folders.node.child.dropRight(1) ++ names.flatMap { name =>
      Text("\n    ") ::
      <element xsi:type="archimate:ApplicationFunction" name={name} id={generateId}/> ::
      Nil
    } ++ Text("\n  ")
    val updatedNodeFolder = folders.node.updateChildren(updatedChildren)
    val updatedXml = folders.update(node = Some(updatedNodeFolder))
    new Archi(updatedXml, fileBegin, random)

  private type RemoveSC = (Seq[XmlNode], Seq[String])

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

  def removeDependencies(dependencies: Iterable[(Node, Node)]): Archi =
    for (from, to) <- dependencies yield
      if (!from.targets.contains(to)) throw Exception(s"No such dependency: ${from.name} -> ${to.name}")
    val idDependencies = dependencies.map((from, to) => (from.id, to.id)).toSet
    val folders: Folders = Folders(xml)

    val (dependencyElements, relationIdSeq) = folders.dependency.child.partitionMap { element =>
      val isTarget = element.label == "element" &&
        idDependencies.contains(element.singleGetAttr("source"), element.singleGetAttr("target"))
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
    new Archi(updatedXml, fileBegin, random)

  def addDependencies(dependencies: Iterable[(Node, Node)]): Archi =
    val folders: Folders = Folders(xml)
    val updatedChildren = folders.dependency.child.dropRight(1) ++ dependencies.toSeq.flatMap { case (from, to) =>
      Text("\n    ") ::
      <element xsi:type="archimate:ServingRelationship" id={generateId} source={from.id} target={to.id}/> ::
      Nil
    } ++ Text("\n  ")
    val updatedDependencyFolder = folders.dependency.updateChildren(updatedChildren)
    val updatedXml = folders.update(dependency = Some(updatedDependencyFolder))
    new Archi(updatedXml, fileBegin, random)

  override def toString: String =
    val orig = xml.toString
    val fixedBegin = fileBegin + orig.substring(orig.indexOf('\n')) + '\n'
    val noEmptyLines = fixedBegin.replaceAll("\\n(\\s*\\n)+", "\n")
    noEmptyLines


object Archi:
  def apply(xml: String): Archi =
    val fileBegin = xml.substring(0, xml.indexOf('\n', xml.indexOf('\n') + 1))
    new Archi(XML.loadString(xml), fileBegin)
