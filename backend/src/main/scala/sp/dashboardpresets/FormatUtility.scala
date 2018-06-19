package sp.dashboardpresets

import play.api.libs.json.{Format, JsObject}
import sp.domain.{JSFormat, JSReads, JSWrites, SPValue}

import scala.util.Try

object FormatUtility {
  def mapWrites[K, V](f: K => String)(implicit writes: JSWrites[V]): JSWrites[Map[K, V]] = xs => {
    JsObject(xs.map { case (k, v) => f(k) -> SPValue(v) })
  }

  def mapReads[K, V](keyParser: String => Option[K])(implicit reads: JSReads[V]): JSReads[Map[K, V]] = { json =>
     json.validate[Map[String, SPValue]]
       .map {
         _.collect { case (k, v) => for (key <- keyParser(k); value <- v.to[V].toOption) yield key -> value }
           .flatten
           .toMap
       }
  }

  /**
    *
    * @param deserializeKey Function to turn a String key into its correct type
    * @param serializeKey Function to serialize the key into a String
    * @param f JSFormat for the map value type
    * @tparam K The map key type
    * @tparam V The map value type
    * @return A JSFormat for Map[K, V]
    *
    *         Usage example:
    *         implicit lazy val fOpenWidgetMap = mapFormat[ID, OpenWidget](k => ID.makeID(k), k => k.toString)
    */
  def mapFormat[K, V](deserializeKey: String => Option[K], serializeKey: K => String)(implicit f: JSFormat[V]): Format[Map[K, V]] = {
    Format(mapReads[K, V](deserializeKey), mapWrites[K, V](serializeKey))
  }

  implicit class SPValueExtension(v: SPValue) {
    def to[T](implicit fjs: JSReads[T]): Try[T] = Try { v.as[T] }
  }
}
