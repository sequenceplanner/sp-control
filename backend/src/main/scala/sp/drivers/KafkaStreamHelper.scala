package sp.drivers

import java.util.UUID

import akka.NotUsed


// TODO 180313 move to sp comm

/**
  * This helper only receives messages that arrives to the topic after it has started
  */
trait KafkaStreamHelper {
  import akka.kafka._
  import akka.kafka.scaladsl._
  import akka.actor._
  import akka.stream.scaladsl._
  import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
  import org.apache.kafka.clients.producer.ProducerRecord
  import org.apache.kafka.common.serialization._
  import akka.stream.ActorMaterializer
  import scala.concurrent._
  import scala.concurrent.duration._
  import akka.Done
  import scala.util.{Failure, Success}

  val system: ActorSystem
  lazy implicit val ec = system.dispatcher
  lazy implicit val materializer = ActorMaterializer.create(system)

  lazy val host: String = system.settings.config getString "sp.kafka.interface"
  lazy val port: Int = system.settings.config getInt "sp.kafka.port"
  lazy val hostName = s"$host:$port"


  lazy val producerSettings = ProducerSettings(system, new ByteArraySerializer, new StringSerializer)
    .withBootstrapServers(hostName)
  lazy val kafkaProducer = producerSettings.createKafkaProducer()

  lazy val consumerSettings = ConsumerSettings(system, new ByteArrayDeserializer, new StringDeserializer)
    .withBootstrapServers(hostName)
    .withGroupId(UUID.randomUUID().toString)
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest")

  def terminateWhenDone(result: Future[Done]): Unit = {
    result.onComplete {
      case Failure(e) =>
        system.log.error(e, e.getMessage)
        system.terminate()
      case Success(_) =>
        println("done")
        system.terminate()
    }
  }


  def consumerFromTopic(topic: String) = Consumer.plainSource(consumerSettings, Subscriptions.topics(topic))
  lazy val producerSink = Producer.plainSink(producerSettings)

  def jsonToProducerRecord(topic: String): Flow[String, ProducerRecord[Array[Byte], String], NotUsed] =
    Flow[String].map(json => new ProducerRecord[Array[Byte], String](topic, json))



}
