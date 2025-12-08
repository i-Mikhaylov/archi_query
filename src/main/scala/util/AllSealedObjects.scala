package util

import scala.compiletime.*


private inline def allInstances[ET <: Tuple, T]: List[T] =
  inline erasedValue[ET] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[ValueOf[t]].value.asInstanceOf[T] :: allInstances[ts, T]

inline def allSealedObjects[T](using m: scala.deriving.Mirror.SumOf[T]): List[T] =
  allInstances[m.MirroredElemTypes, m.MirroredType]
