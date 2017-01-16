package strawman.collection.immutable

import strawman.collection.{Iterable, IterableOps, SeqMonoTransforms, ArrayLike, IndexedView, TagGuidedPolyTransforms, arrayToArrayOps}
import strawman.collection.mutable.{Buildable, ArrayBuffer}

import scala.{Int, AnyVal, AnyRef}
import scala.Predef.String
import scala.reflect.ClassTag

class ArrayOps[A](val xs: Array[A])
  extends AnyRef
                  // Note: we would like this to be a value class over `immutable.Array[A]`,
                  // but scalac complains with
                  //
                  //     error: value class may not wrap another user-defined value class
                  //
                  // This is SI-7685. dotty accepts the value class, however. We should fix
                  // the problem in scalac, fortunately it's scheduled to arrive for 2.13.
                  // Until that is the case, we have no choice but to make ArrayOps a
                  // normal class.
    with IterableOps[A]
    with SeqMonoTransforms[A, Array[A]]
    with TagGuidedPolyTransforms[A, Array]
    with Buildable[A, Array[A]]
    with ArrayLike[A] {

  override def view = new ArrayView(xs)

  protected def coll = view
  def iterator() = coll.iterator()

  def length = xs.length
  def apply(i: Int) = xs.apply(i)

  def elemTag: ClassTag[A] = ClassTag(xs.elemClass)

  protected def fromIterableWithSameElemType(coll: Iterable[A]): Array[A] =
    coll.toArray[A](elemTag).readOnly

  def fromIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B].readOnly

  protected[this] def newBuilder = new ArrayBuffer[A].mapResult(_.toArray(elemTag).readOnly)

  override def knownSize = length

  override def className = "immutable.Array"
}

case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
  def length = xs.length
  def apply(n: Int) = xs(n)
  override def className = "immutable.ArrayView"
}
