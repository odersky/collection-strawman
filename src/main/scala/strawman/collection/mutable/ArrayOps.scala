package strawman.collection.mutable

import strawman.collection.{Iterable, IterableOps, SeqMonoTransforms, ArrayLike, IndexedView, TagGuidedPolyTransforms}
import strawman.collection.immutable
import scala.{Int, Array, AnyVal}
import scala.Predef.String
import scala.reflect.ClassTag

class ArrayOps[A](val xs: Array[A])
  extends AnyVal with IterableOps[A]
    with SeqMonoTransforms[A, Array[A]]
    with TagGuidedPolyTransforms[A, Array]
    with Buildable[A, Array[A]]
    with ArrayLike[A] {

  override def view = new ArrayView(xs)

  protected def coll = view
  def iterator() = coll.iterator()

  def length = xs.length
  def apply(i: Int) = xs.apply(i)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  protected def fromIterableWithSameElemType(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  def fromIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]

  protected[this] def newBuilder = new ArrayBuffer[A].mapResult(_.toArray(elemTag))

  override def knownSize = xs.length

  override def className = "Array"

  def readOnly: immutable.Array[A] = new immutable.Array(xs)
}

case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
  def length = xs.length
  def apply(n: Int) = xs(n)
  override def className = "ArrayView"
}
