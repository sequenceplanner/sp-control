package spgui

import monocle.{Lens, Optional}

import scala.collection.GenTraversableOnce

object SimpleSet {
  def apply[K, V](hashBy: V => K, values: V*) = new SimpleSet[K, V](hashBy, values.map(v => hashBy(v) -> v).toMap)

    /**
      * @return An Optional lens into a SimpleSet for the value associated with the key id.
      */
    def at[K, V](k: K) = Optional[SimpleSet[K, V], V](_.get(k))(v => set => set + v)
    def upsert[K, V](k: K, default: V) = Lens[SimpleSet[K, V], V](_.getOrElse(k, default))(v => set => set.updated(k, v))
}

/**
  * A simple set for handling cases where an object is hashed by some subproperty of that object.
  * Eg. for an object with an ID, it might be the ID. This is a helper class for simplifying working
  * with such a structure. Example use case:
  * {{{
  *   case class Model(id: Int, data: List[Any)
  *   val newModel = Model(123, List())
  *
  *   // Old
  *   val models: Map[Model, Model] = new Map()
  *
  *   // Update
  *   models + (newModel -> newModel)
  *
  *   // New
  *   val models = new SimpleSet(_.id, Map[Int, Model])
  *
  *   // Update
  *   models + newModel
  *
  * }}}
  * @param hashBy Function used to get the object to hash by
  * @param data Initial data for the underlying map
  * @tparam K Key type
  * @tparam V Value type
  */
class SimpleSet[K, V](hashBy: V => K, private val data: Map[K, V]) {
  def +(value: V) = new SimpleSet(hashBy, data + (hashBy(value) -> value))
  def -(value: V) = new SimpleSet(hashBy, data - hashBy(value))
  def removeByKey(key: K) = new SimpleSet(hashBy, data -- Some(key))
  def replace(value: V) = new SimpleSet(hashBy, data.updated(hashBy(value), value))
  def updated(key: K, value: V) = new SimpleSet(hashBy, data.updated(key, value))
  def getByValue(value: V): Option[V] = data.get(hashBy(value))
  def get(key: K): Option[V] = data.get(key)
  def getOrElse(k: K, v: => V): V = data.getOrElse(k, v)
  def apply(key: K): V = data(key)
  def modify(f: V => V)(value: V): SimpleSet[K, V] = modifyByKey(f)(hashBy(value))
  def modifyByKey(f: V => V)(key: K): SimpleSet[K, V] = data.get(key).map(f).fold(this)(updated(key, _))
  def findByKey(f: K => Boolean): Option[V] = data.collectFirst {
    case (k, v) if f(k) => v
  }

  def find(f: V => Boolean): Option[V] = data.collectFirst { case (_, v) if f(v) => v }
  def filter(f: (K, V) => Boolean): SimpleSet[K, V] = {
    val res = data.filter { case (k, v) => f(k, v) }.values.toSeq
    SimpleSet(hashBy, res:_*)
  }
  def collect[B](pf: PartialFunction[V, B]): Iterable[B] = {
    val pf2: PartialFunction[(K, V), B] = {
      case (_, v) => pf(v)
    }

    data.collect(pf2)
  }

  def filterKeys(f: K => Boolean): SimpleSet[K, V] = {
    val res = data.filterKeys(f).values.toSeq
    SimpleSet(hashBy, res:_*)
  }

  def addAll(xs: Iterable[V]): SimpleSet[K, V] = {
    new SimpleSet(hashBy, data ++ xs.map(x => hashBy(x) -> x).toMap)
  }

  def asIterable: Iterable[V] = data.values
  def toList: List[V] = asIterable.toList
  def toSet: Set[V] = asIterable.toSet
  def containsValue(value: V): Boolean = data.get(hashBy(value)).contains(value)
  def contains(key: K): Boolean = data.contains(key)
  def map[B](f: V => B): Iterable[B] = data.values.map(f)
  def flatMap[B](f: V => Iterable[B]): Iterable[B] = data.values.flatMap(f)
  def isEmpty: Boolean = data.isEmpty
  def foreach(f: V => Unit): Unit = data.values.foreach(f)

  override def toString: String = s"SimpleSet(${data.values.mkString(", ")})"
}
