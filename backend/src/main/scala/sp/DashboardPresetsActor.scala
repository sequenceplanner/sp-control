package sp

import akka.actor._
import akka.persistence.{PersistentActor, SnapshotOffer}
import play.api.libs.json.Json
import sp.domain._
import sp.{APIDashboardPresets => API}
import FormatUtility.mapFormat

class DashboardPresetsActor extends PersistentActor with ActorLogging with sp.service.ServiceSupport {
  //final case class State(patients: Map[String, Patient] = Map())

  subscribe(API.DashboardPresetsTopic)


  case class State(presets: Map[String, String] = Map()) {
    def serialize: String = Json.toJson(this).toString()
  }

  implicit val fMap: JSFormat[Map[String, String]] = mapFormat[String, String](x => Some(x), identity)
  implicit val fState: JSFormat[State] = Json.format[State]

  override def receiveCommand: Receive = handleCommand(State())

  def handleCommand(state: State): Receive = {
    case json: String =>
      for {
        message <- SPMessage.fromJson(json)
        event <- message.getBodyAs[API.Event]
        newState <- computeStateFromEvent(state)(event)
      } {
        sendPresets(newState.presets)
        saveSnapshot(newState.serialize)
        context become handleCommand(newState)
      }

    case message => println(s"[DashboardPresetsActor]: Received unknown message: $message")
  }

  def computeStateFromEvent(state: State): PartialFunction[API.Event, Option[State]] = {
    case API.DashboardPresetsRequest =>
      Some(state)

    case API.AddDashboardPreset(name: String, preset: String) =>
      Some(state.copy(presets = state.presets + (name -> preset)))

    case API.RemoveDashboardPreset(name: String) =>
      Some(state.copy(presets = state.presets.filterKeys(_ != name)))

    case API.AllDashboardPresets(presets: Map[String, String]) => // A set of presets has been received. Save them.
      val mergedPresets = presets.foldLeft(state.presets) { case (map, (k, v)) => map.updated(k, v) }
      Some(state.copy(presets = mergedPresets))

    case event =>
      log.warning("Unexpected event type in DashboardPresetsActor: " + event)
      None
  }

  /**
  Publishes a SPMessage on bus with header and body.
    */
  def sendPresets(presets: Map[String, String]) {
    import API.Formats._
    val header = SPHeader(from = "PersistentStorage", to = "DashboardPresetsHandler")
    val message = SPMessage.make(header, API.AllDashboardPresets(presets)).toJson

    publish(
      topic = API.DashboardPresetsTopic,
      json = message
    )
  }

  override def receiveRecover: Receive = {
    case SnapshotOffer(_, snapshot) =>
      for (recoveredState <- SPAttributes.fromJsonGetAs[State](snapshot.toString))
        context become handleCommand(recoveredState)

    case _ => ()
  }

  override def persistenceId: String = "gui-state-persistence-actor"
}

object DashboardPresetsActor {
  def apply() = Props(new DashboardPresetsActor())
}