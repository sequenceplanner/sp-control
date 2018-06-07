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


    /**
      * A request from SP for the human, for example to execute a task.
      * @param name The identification of the human, for the receivers to match
      * @param state The statevariables to change, usually includes cmd, description, etc
      */
    case class StateChangeRequest(name: String, state: Map[String, SPValue]) extends ToHuman

    /**
      * The response from the human interaction services, i.e. a widget or another system
      * @param name The identification of the human, that sends the state
      * @param state The state variables that this sender includes in its state
      */
    case class HumanEvent(name: String, state: Map[String, SPValue]) extends FromHuman




    object Formats {
      import play.api.libs.json._
      implicit val fStartTheTicker: JSFormat[StateChangeRequest] = Json.format[StateChangeRequest]
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

