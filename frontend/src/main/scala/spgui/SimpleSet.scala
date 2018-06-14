package spgui

import scala.collection.GenTraversableOnce

object SimpleSet {
  def apply[K, V](hashBy: V => K, values: V*) = new SimpleSet[K, V](hashBy, values.map(v => hashBy(v) -> v).toMap)
}

class SimpleSet[K, V](hashBy: V => K, private val data: Map[K, V]) {
  def +(value: V) = new SimpleSet(hashBy, data + (hashBy(value) -> value))
  def -(value: V) = new SimpleSet(hashBy, data - hashBy(value))
  def removeByKey(key: K) = new SimpleSet(hashBy, data -- Some(key))
  def replace(value: V) = new SimpleSet(hashBy, data.updated(hashBy(value), value))
  def updated(key: K, value: V) = new SimpleSet(hashBy, data.updated(key, value))
  def getByValue(value: V): Option[V] = data.get(hashBy(value))
  def get(key: K): Option[V] = data.get(key)
  def apply(key: K): V = data(key)
  def modify(f: V => V)(value: V): SimpleSet[K, V] = {
    val key = hashBy(value)
    updated(key, f(data(key)))
  }

  def asIterable: Iterable[V] = data.values
  def toList: List[V] = asIterable.toList
  def toSet: Set[V] = asIterable.toSet
  def containsValue(value: V): Boolean = data.get(hashBy(value)).contains(value)
  def contains(key: K): Boolean = data.contains(key)
  def map[B](f: V => B): GenTraversableOnce[B] = data.values.map(f)
  def flatMap[B](f: V => GenTraversableOnce[B]): Iterable[B] = data.values.flatMap(f)
}
