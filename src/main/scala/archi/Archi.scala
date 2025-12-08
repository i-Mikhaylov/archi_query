package archi

import archi.DiagramChildOps.*
import util.Printer.printWarn

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Random, Try}
import scala.xml.{Attribute, Elem, MetaData, NodeSeq, Null, Text, XML, Node as XmlNode}


class ArchiException(message: String) extends Exception(message)
object ArchiException:
  def apply(message: String) = new ArchiException(message)


private implicit class SingleElementSeq[T](seq: Seq[T]):
  def single(name: String): T = {
    def truncatedName = if (name.length > 10000) name.substring(0, 10000) + "..." else name
    seq match {
      case Seq() => throw ArchiException(s"Not found $truncatedName")
      case Seq(single) => single
      case _ => throw ArchiException(s"Found multiple occurrences of $truncatedName")
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

  def mapAttrs(op: PartialFunction[(String, String), String]): Elem = node match {
    case elem: Elem =>
      val attributes = elem.attributes.foldRight[MetaData](Null) {
        case (attr: Attribute, tail) =>
          val newValue = op.applyOrElse((attr.prefixedKey, attr.value.text), _._2)
          Attribute(attr.pre, attr.key, newValue, tail)
        case (Null, tail) => Null
      }
      elem.copy(attributes = attributes)
    case other => throw ArchiException(s"Can't map attributes on non Elem: $other")
  }

  def updateChildren(children: Seq[XmlNode]): Elem = node match {
    case elem: Elem => elem.copy(child = children)
    case other => throw ArchiException(s"Can't update children on non Elem: $other")
  }

private class Folders(xml: Elem):
  private val all = xml \ "folder"

  def apply(folderType: String): XmlNode =
    all.filter(_.singleGetAttr("type") == folderType).single(folderType.capitalize + " Folder")

  lazy val dependency: XmlNode = apply("relations")
  lazy val node: XmlNode = apply("application")
  lazy val diagram: XmlNode = apply("diagrams")

private object DiagramChildOps:

  private enum ElementType { case Child, Group, Other }
  private def getType(element: XmlNode) =
    def xsiType = element.singleGetAttr("xsi:type")
    element.label match
      case "element" => xsiType match
        case "archimate:ArchimateDiagramModel"  => ElementType.Group
        case _                                  => ElementType.Other
      case "child" => xsiType match
        case "archimate:Group"                  => ElementType.Group
        case "archimate:DiagramObject"          => ElementType.Child
        case _                                  => ElementType.Other
      case _                                    => ElementType.Other

  def diagramChildFilter(children: Seq[XmlNode])(predicate: XmlNode => Boolean): Seq[XmlNode] =
    children.flatMap { element =>
      getType(element) match
        case ElementType.Child => Option.when(predicate(element))(element)
        case ElementType.Group => Some(element updateChildren diagramChildFilter(element.child)(predicate))
        case ElementType.Other => Some(element)
    }
  def diagramChildMap(children: Seq[XmlNode])(transform: XmlNode => XmlNode): Seq[XmlNode] =
    children.map { element =>
      getType(element) match
        case ElementType.Child => transform(element)
        case ElementType.Group => element updateChildren diagramChildMap(element.child)(transform)
        case ElementType.Other => element
    }
  def diagramChildMapExtract[Acc](children: Seq[XmlNode])(transform: XmlNode => (XmlNode, Seq[Acc])): (Seq[XmlNode], Seq[Acc]) =
    children.flatMap { element =>
      getType(element) match
        case ElementType.Child =>
          val (updatedElement, acc) = transform(element)
          Left(updatedElement) :: acc.map(Right(_)).toList
        case ElementType.Group =>
          val (updatedChildren, acc) = diagramChildMapExtract(element.child)(transform)
          Left(element updateChildren updatedChildren) :: acc.map(Right(_)).toList
        case ElementType.Other => Some(Left(element))
    }.partitionMap(identity)


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
      .withDefault(key => throw ArchiException(s"Module with key $key not found"))

  lazy val byName: Map[String, Node] =
    byId.values.map(node => node.name -> node).toMap.withDefault(name => throw ArchiException(s"Module $name not found"))

  lazy val projects: List[Node] = byId.values.filter(_.isProject).toList

  implicit def nameToNode(name: String): Node = byName(name)


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


  private type OptChildren = Option[Seq[XmlNode]]
  private def update(dependencies: OptChildren = None, nodes: OptChildren = None, diagrams: OptChildren = None): Archi =
    val updateMap = {
      dependencies.map("relations" -> _).toList :::
        nodes.map("application" -> _).toList :::
        diagrams.map("diagrams" -> _).toList
    }.toMap
    val updatedChildren = xml.child.map { folder =>
      Option.when(folder.label == "folder")(folder.singleGetAttr("type"))
        .flatMap(updateMap.get)
        .fold(folder)(folder.updateChildren)
    }
    new Archi(xml.updateChildren(updatedChildren), fileBegin, random)


  private case class RemoveDepResult(dependencies: Seq[XmlNode], diagrams: Seq[XmlNode])


  private def removeDependenciesInner(dependencies: Iterable[(Node, Node)], folders: Folders): RemoveDepResult =
    for (from, to) <- dependencies yield
      if (!from.targets.contains(to))
        throw ArchiException(s"No such dependency: ${from.name} -> ${to.name}")
    val idDependencies = dependencies.map((from, to) => (from.id, to.id)).toSet

    val (dependencyChildren, relationIdSeq) = folders.dependency.child.partitionMap { element =>
      def toRemove = idDependencies.contains(element.singleGetAttr("source"), element.singleGetAttr("target"))
      Either.cond(element.label == "element" && toRemove, element.singleGetAttr("id"), element)
    }
    val relationIds = relationIdSeq.toSet

    val (diagramChildren1, sourceConnectionIdSeq) = diagramChildMapExtract(folders.diagram.child) { child =>
      val (updatedGrandChildren, sourceConnectionIds) = child.child.partitionMap { grandChild =>
        if (grandChild.label != "sourceConnection")
          Left(grandChild)
        else if (grandChild.singleGetAttr("xsi:type") != "archimate:Connection")
          throw ArchiException(s"Found unknown xsi:type: $grandChild")
        else if (relationIds contains grandChild.singleGetAttr("archimateRelationship"))
          Right(grandChild.singleGetAttr("id"))
        else Left(grandChild)
      }
      child.updateChildren(updatedGrandChildren) -> sourceConnectionIds
    }
    val sourceConnectionIds = sourceConnectionIdSeq.toSet

    val diagramChildren2 = diagramChildMap(diagramChildren1) { child =>
      child.mapAttrs { case ("targetConnections", idStr) =>
        idStr.split(' ').filterNot(sourceConnectionIds.contains).mkString(" ")
      }
    }

    RemoveDepResult(dependencyChildren, diagramChildren2)


  def removeModules(modules: Iterable[Node]): Archi =
    for module <- modules yield
      if (!byId.contains(module.id))
        throw ArchiException(s"Node ${module.name} not found")

    val moduleIds = modules.map(_.id).toSet
    val dependencies = modules
      .flatMap { module => module.sources.map { _ -> module } ::: module.targets.map { module -> _ } }
    
    val folders = Folders(xml)
    val updatedNodes = folders.node.child.filter {
      case element: Elem => element.label != "element" || !moduleIds.contains(element.singleGetAttr("id"))
      case other => true
    }
    val removedDeps = removeDependenciesInner(dependencies, folders)
    val diagrams = diagramChildFilter(removedDeps.diagrams) { child =>
      !moduleIds.contains(child.singleGetAttr("archimateElement"))
    }

    update(dependencies = Some(removedDeps.dependencies), nodes = Some(updatedNodes), diagrams = Some(diagrams))

  def renameModules(renameList: Iterable[(Node, String)]): Archi =
    val renameMap = renameList.map((node, newName) => node.name -> newName).toMap
    val updated = Folders(xml).node.child.map {
      case element: Elem if element.label == "element" =>
        renameMap.get(element.singleGetAttr("name")).fold(element)
          { newName => element.mapAttrs { case ("name", _) => newName } }
      case other => other
    }
    update(nodes = Some(updated))

  def addModules(names: Iterable[String]): Archi =
    val updated = Folders(xml).node.child.dropRight(1) ++ names.flatMap { name =>
      Text("\n    ") ::
      <element xsi:type="archimate:ApplicationFunction" name={name} id={generateId}/> ::
      Nil
    } ++ Text("\n  ")
    update(nodes = Some(updated))

  def removeDependencies(dependencies: Iterable[(Node, Node)]): Archi =
    val updated = removeDependenciesInner(dependencies, Folders(xml))
    update(dependencies = Some(updated.dependencies), diagrams = Some(updated.diagrams))

  def addDependencies(dependencies: Iterable[(Node, Node)]): Archi =
    val updated = Folders(xml).dependency.child.dropRight(1) ++ dependencies.toSeq.flatMap { case (from, to) =>
      Text("\n    ") ::
      <element xsi:type="archimate:ServingRelationship" id={generateId} source={from.id} target={to.id}/> ::
      Nil
    } ++ Text("\n  ")
    update(dependencies = Some(updated))


  override def toString: String =
    val orig = xml.toString
    val fixedBegin = fileBegin + orig.substring(orig.indexOf('\n')) + '\n'
    val noEmptyLines = fixedBegin.replaceAll("\\n(\\s*\\n)+", "\n")
    noEmptyLines


object Archi:
  def apply(xml: String): Archi =
    val fileBegin = xml.substring(0, xml.indexOf('\n', xml.indexOf('\n') + 1))
    new Archi(XML.loadString(xml), fileBegin)


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
    getPathInner(from, List(to :: Nil), Nil)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .map(NodePath(_, _))
      .toList
      .sorted
