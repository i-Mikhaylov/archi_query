import java.nio.file.{Files, Paths}


val archiSrc = "uses.archimate"
val archiDst = "uses2.archimate" //"../c4enterprise/uses.archimate"

@main
def main(): Unit =
  val archi = Archi(Files.readString(Paths.get(archiSrc)))
  val nodeByName = archi.nodes.values.map(node => node.name -> node).toMap
  //val projects = archi.nodes.values.filter(_.nodeType == Node.Project).toList

  val roadUnitNode = nodeByName("c4.mod.domain.c4roadunit")
  val srlNode = nodeByName("c4.mod.c4srl.base")
  val updated = archi.removeRelations(Set(srlNode.id -> roadUnitNode.id))
  Files.writeString(Paths.get(archiDst), updated.toString)



  /*val List(pathsToDomain, pathsToBase) = List(
    "c4.mod.domain.c4roadunit",
    "c4.cargo.c4roadunit.base",
  ).map { name =>
    val getPath = Node.pathToChild(nodeByName(name).id)
    projects
      .map { node => node.id -> getPath(node) }
      .filter(_._2.nonEmpty)
      .toMap
  }

  println(pathsToDomain.size)
  println(pathsToBase.size)

  val diff = pathsToDomain.keySet diff pathsToBase.keySet

  println(diff.head)
  println(Archi.NodePath(pathsToDomain(diff.head)))*/

  //println(archi.nodes.values.find(_.name == "c4.mod.wtms.c4job").get.children.map(_.name).mkString("\n"))

  //println(archi.nodes.values.find(_.name == "c4.mod.domain.c4roadunit").get.graphvizChildren)

