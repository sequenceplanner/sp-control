package sp.driver{
  import sp.domain.Logic._
  import sp.domain._

  // this object is used for converting to and from json.
  object APIHumanDriver {
    sealed trait ToHuman
    sealed trait FromHuman
    val service = "HumanDriver"
    val topicToHuman = "humanDriverToHuman"
    val topicFromHuman = "humanDriverFromHuman"


    case class HumanStateMessage(driverID: ID,
                                 humanName: String,
                                 humanID: String,
                                 loggedIn: Boolean,
                                 cmd: String,
                                 ack: Boolean,
                                 done: Boolean,
                                 bluetooth: Boolean,
                                 instructions: Map[String, String]
                              ) extends ToHuman


    case class HumanEvent(driverID: ID,
                          ack: Boolean,
                          done: Boolean) extends FromHuman




    object Formats {
      import play.api.libs.json._
      implicit val fStartTheTicker: JSFormat[HumanStateMessage] = Json.format[HumanStateMessage]
      implicit val fStopTheTicker: JSFormat[HumanEvent] = Json.format[HumanEvent]
      def fHumanDriverRequest: JSFormat[ToHuman] = Json.format[ToHuman]
      def fHumanDriverResponse: JSFormat[FromHuman] = Json.format[FromHuman]
    }
    object ToHuman {
      implicit lazy val fHumanDriverRequest: JSFormat[ToHuman] = Formats.fHumanDriverRequest
    }
    object FromHuman {
      implicit lazy val fHumanDriverResponse: JSFormat[FromHuman] = Formats.fHumanDriverResponse
    }

  }

}

