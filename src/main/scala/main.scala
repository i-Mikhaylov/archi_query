import java.nio.file.{Files, Paths}


@main
def main(): Unit =
  val archi = Archi(Files.readString(Paths.get("/home/ivan/c4repo/c4enterprise/uses.archimate")))
  val nodeByName = archi.nodes.values.map(node => node.name -> node).toMap
  val projects = archi.nodes.values.filter(_.nodeType == Node.Project).toList

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

  println(archi.nodes("d76016c8").graphvizChildren)

