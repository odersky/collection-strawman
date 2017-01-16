package strawman.collection.immutable

import java.lang.Class
import scala.{AnyVal, Int}
import scala.reflect.ClassTag
import strawman.collection.arrayToArrayOps

class Array[T](private val underlying: scala.Array[T]) extends AnyVal {
  def length: Int = underlying.length
  def apply(i: Int): T = underlying.apply(i)
  def elemClass: Class[_] = underlying.getClass.getComponentType
}

object Array {
  def apply[T: ClassTag](elems: T*): Array[T] =
    scala.Array(elems: _*).readOnly
}
