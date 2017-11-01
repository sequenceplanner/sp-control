package sp.operationmatcher {

  import sp.domain._
  import sp.abilityhandler.APIAbilityHandler.{Ability => Ability}

  object API {
    val service = "OperationMatcher"
    val topicRequest = service+"Requests"
    val topicResponse = service+"Response"

    sealed trait Request
    sealed trait Response

    case class Find(pairs: Map[String, SPValue]) extends Request
    case class Matches(abilities: List[Ability], neighbors: List[Ability]) extends Response

    object Formats {
      import play.api.libs.json._
      import sp.abilityhandler.APIAbilityHandler.Formats._
      implicit lazy val fFind: JSFormat[Find] = Json.format[Find]
      implicit lazy val fMatches: JSFormat[Matches] = Json.format[Matches]
      def fOperationMatcherRequest: JSFormat[Request] = Json.format[Request]
      def fOperationMatcherResponse: JSFormat[Response] = Json.format[Response]
    }

    object Request {
      implicit lazy val fOperationMatcherRequest: JSFormat[Request] = Formats.fOperationMatcherRequest
    }

    object Response {
      implicit lazy val fOperationMatcherResponse: JSFormat[Response] = Formats.fOperationMatcherResponse
    }
  }

}
