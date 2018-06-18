package sp

import sp.domain.JSFormat


object APIHumanGUI {

  sealed trait Request
  sealed trait Response
  val service = "HumanService"
  val topicRequest = "HumanServiceRequests"
  val topicResponse = "HumanServiceResponse"

  case class InputUserDetails(name:String , id:String)extends Request
  case class UserDetails(name:String , id:String) extends Response


  // The below is an example how to create the json formaters
  object Formats {

    import play.api.libs.json._


    implicit val fUserDetails: JSFormat[ UserDetails] = Json.format[ UserDetails]
    implicit val fInputUserDetails: JSFormat[ InputUserDetails] = Json.format[ InputUserDetails]
  }

}
