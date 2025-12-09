package util

import scala.annotation.tailrec
import scala.util.{Random, Try}


class IdGenerator private(random: Random, existingIds: Set[Int]):
  @tailrec final def generate: String =
    val next = random.nextInt
    if existingIds.contains(next) then generate
    else f"$next%08x"

object IdGenerator:
  def apply(seed: AnyRef, existingIds: Iterable[String]): IdGenerator =
    val random = new Random(seed.hashCode)
    val existingSet = existingIds
      .flatMap { id => Try(Integer.parseUnsignedInt(id, 16)).toOption }
      .toSet
    new IdGenerator(random, existingSet)
