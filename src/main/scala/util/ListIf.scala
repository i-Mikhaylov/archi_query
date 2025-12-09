package util

object ListIf:
  def apply[T](condition: Boolean)(value: => T): List[T] =
    if condition then value :: Nil else Nil
