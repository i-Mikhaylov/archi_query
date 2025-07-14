import java.nio.file.{Files, Paths}


private val archiSrc = "uses.archimate"
private val archiDst = "../c4enterprise/uses.archimate"

private val toRemoveDependencies = List(
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4roadunit",
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4cnt.base",
  "c4.mod.domain.c4decision" -> "c4.mod.domain.c4roadunit",
  "c4.mod.domain.c4decision" -> "c4.mod.domain.c4cnt.base",
)
private val toAddDependencies = List(
  "c4.mod.domain.c4techflow" -> "c4.mod.domain.c4cnt.base",
)


private val archi = Archi(Files.readString(Paths.get(archiSrc)))
private val rich = ArchiRich(archi)
private val byName = rich.nodeByName


def rewrite(): Unit =
  val archi = Archi(Files.readString(Paths.get(archiSrc)))
  val rich = ArchiRich(archi)
  val byName = rich.nodeByName

  val toRemoveIds = toRemoveDependencies
    .map((fromName, toName) => byName(fromName).id -> byName(toName).id)
  val toAddIds = toAddDependencies
    .map((fromName, toName) => byName(fromName).id -> byName(toName).id)
  val updated = archi.removeDependencies(toRemoveIds.toSet).addDependencies(toAddIds)
  Files.writeString(Paths.get(archiDst), updated.toString)



@main def main(): Unit =
  rewrite()

  /*val module1 = "c4.mod.domain.c4roadunit"
  val module2 = "c4.cargo.c4roadunit.base"
  println(rich.moduleProjectDiff(module1, module2))*/


  //println(Node.pathToChild(byName("c4.mod.domain.c4techflow").id)(byName("c4.mod.domain.c4cnt.base")).beautifulString)


  //println(archi.nodes.values.find(_.name == "c4.mod.wtms.c4job").get.children.map(_.name).mkString("\n"))

  //println(archi.nodes.values.find(_.name == "c4.mod.domain.c4roadunit").get.graphvizChildren)

