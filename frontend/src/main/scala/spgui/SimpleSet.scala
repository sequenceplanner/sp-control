package spgui

import scala.collection.GenTraversableOnce

object SimpleSet {
  def apply[K, V](hashBy: V => K, values: V*) = new SimpleSet[K, V](hashBy, values.map(v => hashBy(v) -> v).toMap)
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
  def apply(key: K): V = data(key)
  def modify(f: V => V)(value: V): SimpleSet[K, V] = modifyByKey(f)(hashBy(value))
  def modifyByKey(f: V => V)(key: K): SimpleSet[K, V] = updated(key, f(data(key)))
  def findByKey(f: K => Boolean): Option[V] = data.collectFirst {
    case (k, v) if f(k) => v
  }

  def find(f: V => Boolean): Option[V] = data.collectFirst {
    case (k, v) if f(v) => v
  }

  def addAll(xs: Iterable[V]) = {
    new SimpleSet(hashBy, data ++ xs.map(x => hashBy(x) -> x).toMap)
  }

  def asIterable: Iterable[V] = data.values
  def toList: List[V] = asIterable.toList
  def toSet: Set[V] = asIterable.toSet
  def containsValue(value: V): Boolean = data.get(hashBy(value)).contains(value)
  def contains(key: K): Boolean = data.contains(key)
  def map[B](f: V => B): GenTraversableOnce[B] = data.values.map(f)
  def flatMap[B](f: V => GenTraversableOnce[B]): Iterable[B] = data.values.flatMap(f)
  def isEmpty: Boolean = data.isEmpty
}
