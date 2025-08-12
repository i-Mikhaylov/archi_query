import scala.collection.IterableOps
import scala.reflect.ClassTag


class ByClassMap[T, I[U] <: IterableOps[U, I, I[U]]](it: I[T]):
  private val map = it.groupBy(_.getClass).withDefaultValue(it.empty)
  def get[U <: T](implicit tag: ClassTag[U]): I[U] = map(tag.runtimeClass.asInstanceOf[Class[U]]).asInstanceOf[I[U]]
