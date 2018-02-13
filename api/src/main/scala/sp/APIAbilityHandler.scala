package sp.abilityhandler

import sp.domain.Logic._
import sp.domain._

  object APIAbilityHandler {
    sealed trait Request
    sealed trait Response
    val service = "abilityHandler"
    val topicRequest = "abilityHandlerRequest"
    val topicResponse = "abilityHandlerRespponse"

    final case class StartAbility(id: ID, params: Map[ID, SPValue] = Map(), attributes: SPAttributes = SPAttributes()) extends Request
    final case class ForceResetAbility(id: ID) extends Request
    case object ForceResetAllAbilities extends Request

    // to be used when handshake is on
    final case class ExecuteCmd(cmd: ID) extends Request

    case object GetAbilities extends Request
    final case class SetUpAbility(ability: Ability, handshake: Boolean = false) extends Request



    final case class CmdID(cmd: ID) extends Response
    final case class AbilityStarted(id: ID) extends Response
    final case class AbilityCompleted(id: ID, result: Map[ID, SPValue]) extends Response
    final case class AbilityState(id: ID, state: Map[ID, SPValue]) extends Response
    final case class Abilities(xs: List[Ability]) extends Response
    final case class Abs(a: List[(ID,String)]) extends Response

    final case class Ability(name: String,
                             id: ID = ID.newID,
                             preCondition: Condition = Condition(AlwaysFalse, List()),
                             started: Condition = Condition(AlwaysFalse, List()),
                             postCondition: Condition = Condition(AlwaysTrue, List()),
                             resetCondition: Condition = Condition(AlwaysTrue, List()),
                             parameters: List[ID] = List(),
                             result: List[ID] = List(),
                             attributes: SPAttributes = SPAttributes())

    /**
      * Use this to convert from abilities to operations for storing in a model
      * @param a The ability
      * @return an operation
      */
    def abilityToOperation(a: Ability): Operation = {
      Operation(
        name = a.name,
        id = a.id,
        attributes = a.attributes ++ SPAttributes(
          "isa" -> "Ability",
          "parameters" -> a.parameters,
          "result" -> a.result
        ),
        conditions = List(
          a.preCondition.copy(attributes = a.attributes ++ SPAttributes("kind" -> "pre")),
          a.started.copy(attributes = a.attributes ++ SPAttributes("kind" -> "started")),
          a.postCondition.copy(attributes = a.attributes ++ SPAttributes("kind" -> "post")),
          a.resetCondition.copy(attributes = a.attributes ++ SPAttributes("kind" -> "reset"))
        )
      )
    }

    /**
      * Converts any operation to an ability
      * @param o
      */
    def operationToAbility(o: Operation): Ability = {
      val p = o.attributes.getAs[List[ID]]("parameters").getOrElse(List())
      val r = o.attributes.getAs[List[ID]]("result").getOrElse(List())
      Ability(
        name = o.name,
        id = o.id,
        parameters = p,
        result = r,
        preCondition = mergeConditions(extractCondition(o, "pre")),
        started = mergeConditions(extractCondition(o, "started")),
        postCondition = mergeConditions(extractCondition(o, "post")),
        resetCondition = mergeConditions(extractCondition(o, "reset"))
      )
    }


    // Move below funtion to OperaitonLogic
    def extractCondition(o: Operation, kind: String): List[Condition] = {
      o.conditions.filter(c => c.attributes.getAs[String]("kind").contains(kind))
    }
    def mergeConditions(xs: List[Condition]): Condition = {
      val init = xs.headOption.getOrElse(Condition(AlwaysFalse))
      xs.tail.foldLeft(init){(a, b) => a.copy(guard = AND(List(a.guard, b.guard)), action = a.action ++ b.action)}
    }




    object Formats {
      import play.api.libs.json._
      implicit lazy val fAbility: JSFormat[Ability] = Json.format[Ability]
      implicit lazy val fStartAbility: JSFormat[StartAbility] = Json.format[StartAbility]
      implicit lazy val fForceResetAbility: JSFormat[ForceResetAbility] = Json.format[ForceResetAbility]
      implicit lazy val fForceResetAllAbilities:     JSFormat[ForceResetAllAbilities.type]     = deriveCaseObject[ForceResetAllAbilities.type]
      implicit lazy val fExecuteCmd: JSFormat[ExecuteCmd] = Json.format[ExecuteCmd]
      implicit lazy val fGetAbilities:     JSFormat[GetAbilities.type]     = deriveCaseObject[GetAbilities.type]
      implicit lazy val fSetUpAbility: JSFormat[SetUpAbility] = Json.format[SetUpAbility]
      implicit lazy val fCmdID: JSFormat[CmdID] = Json.format[CmdID]
      implicit lazy val fAbilityStarted: JSFormat[AbilityStarted] = Json.format[AbilityStarted]
      implicit lazy val fAbilityCompleted: JSFormat[AbilityCompleted] = Json.format[AbilityCompleted]
      implicit lazy val fAbilityState: JSFormat[AbilityState] = Json.format[AbilityState]
      implicit lazy val fAbilities: JSFormat[Abilities] = Json.format[Abilities]
      implicit lazy val fAbs: JSFormat[Abs] = Json.format[Abs]
      def fAbilityHandlerRequest: JSFormat[Request] = Json.format[Request]
      def fAbilityHandlerResponse: JSFormat[Response] = Json.format[Response]
    }


    object Request {
      implicit lazy val fAPIAbilityHandlerRequest: JSFormat[Request] = Formats.fAbilityHandlerRequest
    }

    object Response {
      implicit lazy val fAPIAbilityHandlerResponse: JSFormat[Response] = Formats.fAbilityHandlerResponse
    }
  }
