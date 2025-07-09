import java.nio.file.{Files, Paths}


val archiSrc = "uses.archimate"
val archiDst = "../c4enterprise/uses.archimate"

val toRemoveDependencies: List[(String, String)] = List(
  "c4.mod.c4srl.base" -> "c4.mod.domain.c4roadunit",
  "c4.mod.domain.c4decision" -> "c4.mod.domain.c4roadunit",
  "c4.mod.domain.c4decision" -> "c4.mod.domain.c4cnt.base",
)


@main
def main(): Unit =
  val archi = Archi(Files.readString(Paths.get(archiSrc)))
  val rich = ArchiRich(archi)

  val module1 = "c4.mod.domain.c4roadunit"
  val module2 = "c4.cargo.c4roadunit.base"
  val (diff1, diff2) = rich.moduleProjectDiff(module1, module2)
  println(diff1.map(_.head.name))
  println(diff2.map(_.head.name))

  /*val toRemoveIds = toRemoveDependencies
    .map((fromName, toName) => nodeByName(fromName).id -> nodeByName(toName).id)
    .toSet
  val updated = archi.removeDependencies(toRemoveIds)
  Files.writeString(Paths.get(archiDst), updated.toString)*/



  //println(archi.nodes.values.find(_.name == "c4.mod.wtms.c4job").get.children.map(_.name).mkString("\n"))

  //println(archi.nodes.values.find(_.name == "c4.mod.domain.c4roadunit").get.graphvizChildren)

