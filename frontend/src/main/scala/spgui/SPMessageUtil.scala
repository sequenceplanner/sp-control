package spgui

import sp.domain.{JSReads, SPHeader, SPMessage}

object SPMessageUtil {

  /**
    * Utility class for parsing an SPMessage as one of several types.
    * Example usage:
    *
    * {{{
    *   case class Square(w: Int, h: Int)
    *   case class Circle(r: Int)
    *   case class Line(p1: (Int, Int), p2: (Int, Int))
    *
    *   // A message with a body of Square, Circle or Line
    *   SPMessage message = ???
    *
    *   val res = message.oneOf[Square].or[Circle].or[Line]
    *     .body match {
    *       case Square(w, h) => println(s"Square with (w: $w, h: $h))
    *       case Circle(r) => println(s"Circle with (r: $r)")
    *       case Line((x1, y1), (x2, y2)) => println(s"Line from ($x1, $y1) to ($x2, $y2)")
    *     }
    * }}}
    */
  implicit class BetterSPMessage(message: SPMessage) {
    def as[T](implicit reads: JSReads[T]): Option[(SPHeader, T)] = for {
      header <- message.getHeaderAs[SPHeader]
      body <- message.getBodyAs[T]
    } yield (header, body)

    def oneOf[A](implicit reads: JSReads[A]): OneOf[SPHeader, A] = OneOf[SPHeader, A](message)

    case class OneOf[H, A](message: SPMessage, prevValues: List[Any] = Nil)(implicit reads: JSReads[A], headerReads: JSReads[H]) {
      val header: Option[H] = message.getHeaderAs[H]
      val values: List[Any] = message.getBodyAs[A] match {
        case Some(v) => v :: prevValues
        case None => prevValues
      }

      def or[B](implicit bReads: JSReads[B]): OneOf[H, B] = OneOf[H, B](message, values)
      def body: Option[Any] = values match {
        case Nil => None
        case h :: _ => Some(h)
      }

      def foreach(f: Any => Unit): Unit = body.foreach(f)
      def map[B](f: Any => B): Option[B] = body.map(f)
      def isEmpty[B](f: Any => B): Boolean = body.isEmpty
      def isDefined[B](f: Any => B): Boolean = body.isDefined
      def nonEmpty[B](f: Any => B): Boolean = body.nonEmpty
    }
  }
}
