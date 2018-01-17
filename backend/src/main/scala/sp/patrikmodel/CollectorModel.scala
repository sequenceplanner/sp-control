package sp.patrikmodel

import sp.domain.{ID, _}
import sp.domain.Logic._

import scala.collection.mutable.LinkedHashMap

/**
 * To store operation models
 */

trait CollectorModel extends CollectorImplicits {
  val modelName: String

  def v(name: String, domain: Seq[String] = Seq(), init: Option[String] = None, marked: Set[String] = Set(), idleValue: Option[String] = None, attributes: SPAttributes = SPAttributes()) = {
    variableSet += Thing(name = name, attributes = attributes merge SPAttributes("markings" -> (if (marked.isEmpty) None: Option[Set[String]] else Some(marked)),
      "idleValue" -> idleValue,
      "stateVariable" -> SPAttributes(
        "domain" -> (domain ++ (if (init.isDefined) Seq(init.get) else Seq()) ++ marked.toSeq ++ (if (idleValue.isDefined) Seq(idleValue.get) else Seq())).distinct,
        "init" -> init,
        "goal" -> (if (marked.size == 1) marked.head else None: Option[Int])
      )))
  }

  implicit def SPAttToSeqOfSpAtt(spa: SPAttributes): Seq[SPAttributes] = Seq(spa)
  implicit def stringStringToSeqOfSpAtt(kv: (String, String)): SPAttributes = SPAttributes(kv._1 -> SPValue(kv._2))

  def op(name: String, attributes: Seq[SPAttributes] = Seq(SPAttributes())) = {
    operationSet += Operation(name = name, attributes = attributes.foldLeft(SPAttributes()) { case (acc, c) => acc merge c })
  }
  def opWithID(name: String, attributes: Seq[SPAttributes] = Seq(SPAttributes()), iD: ID) = {
    operationSetWithID += iD -> Operation(name = name, List(), attributes = attributes.foldLeft(SPAttributes()) { case (acc, c) => acc merge c }, id = iD)
    operations = operationSetWithID.map(_._2).toList
  }

  def c(variable: String, fromValue: String, toValue: String): SPAttributes = {
    SPAttributes("preGuard" -> Set(s"$variable == $fromValue"), "preAction" -> Set(s"$variable = $toValue"))
  }
  def c(variable: String, fromValue: String, inBetweenValue: String, toValue: String): SPAttributes = {
    c(variable, fromValue, inBetweenValue) merge SPAttributes("postGuard" -> Set(s"$variable == $inBetweenValue"), "postAction" -> Set(s"$variable = $toValue"))
  }
  def aResourceTrans(resource: String, atStart: String, atExecute: String, atComplete: String): (String, AttributeWrapper) = {
    "resourceTrans" -> SPAttributes(resource -> SPAttributes("atStart" -> atStart, "atExecute" -> atExecute, "atComplete" -> atComplete))
  }
  def aCarrierTrans(carrier: String, atStart: Option[String] = None, atExecute: Option[String] = None, atComplete: Option[String] = None): (String, AttributeWrapper) = {
    "carrierTrans" -> SPAttributes(carrier ->
      SPAttributes("atStart" -> atStart, "atExecute" -> atExecute, "atComplete" -> atComplete)
      //        .foldLeft(Seq(): Seq[(String, String)]) {
      //        case (acc, (key, optValue)) => acc ++ (if (optValue.isDefined) Seq(key -> optValue.get) else Seq())
      //      })
    )
  }

  def x(name: String, forbiddenExpressions: Set[String] = Set(), operations: Set[String] = Set(), attributes: SPAttributes = SPAttributes()) = {
    forbiddenExpressionSet += SOPSpec(name = name, sop = List(), attributes = attributes merge SPAttributes(
      "forbiddenExpressions" -> forbiddenExpressions,
      "mutexOperations" -> operations))
  }

  def createMoveOperations(robotNamePrefix: String = "v", robotName: String, robotNameSuffix: String = "_pos", staticRobotPoses: Map[String, Set[String]]) = {
    staticRobotPoses.foreach {
      case (source, targets) =>
        targets.foreach { target =>
          val inBetweenValue = s"${source}To${target.capitalize}"
          val robot_pos = s"$robotNamePrefix$robotName$robotNameSuffix"
          op(s"${inBetweenValue}_$robotName", c(robot_pos, s"$source", inBetweenValue, s"$target"))
          v(robot_pos, domain = Seq(s"$source", inBetweenValue, s"$target"))
        }

    }
  }

  def robotMovements(robotNamePrefix: String = "v", robotName: String, robotNameSuffix: String = "_pos", staticRobotPoses: SPAttributes, attributes: SPAttributes = SPAttributes()) = {
    robotMovementsSet += SPSpec(name = robotName, attributes = attributes merge SPAttributes(
      "robotNamePrefix" -> robotNamePrefix,
      "robotName" -> robotName,
      "robotNameSuffix" -> robotNameSuffix,
      "staticRobotPoses" -> staticRobotPoses))
  }

  implicit def stringToOption: String => Option[String] = Some(_)
  implicit def stringToSetOfStrings: String => Set[String] = Set(_)
  implicit def stringToSeqOfStrings: String => Seq[String] = Seq(_)
}

trait CollectorImplicits extends ExtendIDables {
  var variableSet: Set[Thing] = Set()
  var operationSet: Set[Operation] = Set()
  var operations : List[Operation] = List()
  var operationSetWithID  = LinkedHashMap[ID,Operation]()
  var forbiddenExpressionSet: Set[SOPSpec] = Set()
  var robotMovementsSet: Set[SPSpec] = Set()

  private def getIDablesFromSet[T <: IDAble](idableSet: Set[T], constructor: (String, SPAttributes) => T) = {
    lazy val idables = idableSet.groupBy(_.name).map { case (k, objs) => k -> objs.foldLeft(SPAttributes()) { case (acc, obj) => acc merge obj.attributes } }
    idables.map(kv => constructor(kv._1, kv._2)).toList
  }

  private def fixDomain(attr: SPAttributes): play.api.libs.json.JsObject = {
    play.api.libs.json.JsObject(attr.fields.map {
      case ("domain", v) =>
        val nd = v.to[List[String]].getOrElse(List()).distinct
        ("domain", play.api.libs.json.JsArray(nd.map(play.api.libs.json.JsString(_))))
      case (k, v) =>
        if(v.to[SPAttributes].toOption.isEmpty)
          (k, v)
        else
          (k, fixDomain(v.to[SPAttributes].toOption.get).to[play.api.libs.json.JsValue].get)
    })
  }

  def parseToIDables() = {
    //Variables-----------------------------------------------------------------------------------------------------
    lazy val varsToAddWithNonDistinctDomains = getIDablesFromSet(variableSet, (n, as) => Thing(name = n, attributes = as))
    lazy val varsToAdd = varsToAddWithNonDistinctDomains.map { obj =>
      obj.copy(attributes = fixDomain(obj.attributes))
      // obj.attributes.transformField { case ("domain", JArray(vs)) => ("domain", JArray(vs.distinct)) }.to[SPAttributes].getOrElse(SPAttributes()))
    }

    //Operations------------------------------------------------------------------------------------
    lazy val opsToAdd = getIDablesFromSet(operationSet, (n, as) => Operation(name = n, attributes = as))
    lazy val operationMap = opsToAdd.map(o => o.name -> o).toMap

    //ForbiddenExpressions--------------------------------------------------------------------------------------
    lazy val fesToAddWithNoVisableOperations = getIDablesFromSet(forbiddenExpressionSet, (n, as) => SOPSpec(name = n, sop = List(), attributes = as))
    lazy val fesToAdd = fesToAddWithNoVisableOperations.map { obj =>
      val mutexOperations = obj.attributes.findAs[List[String]]("mutexOperations").flatten
      obj.copy(sop = if (mutexOperations.isEmpty) List() else List(Arbitrary(mutexOperations.flatMap(o => operationMap.get(o)).map(o => OperationNode(o.id)))),
        attributes = play.api.libs.json.JsObject(obj.attributes.fields.filter(_._1 != "mutexOperations")))
    }

    //RobotMovements---------------------------------------------------------------------------------------------
    lazy val robotMovementsToAdd = getIDablesFromSet(robotMovementsSet, (n, as) => SPSpec(name = n, attributes = as))

    //Return--------------------------------------------------------------------------------------------
    val ids = varsToAdd ++ opsToAdd ++ fesToAdd ++ robotMovementsToAdd
    extendIDables(ids)
  }

  def parseToIDablesWithIDs() = {
    //Variables-----------------------------------------------------------------------------------------------------
    lazy val varsToAddWithNonDistinctDomains = getIDablesFromSet(variableSet, (n, as) => Thing(name = n, attributes = as))
    lazy val varsToAdd = varsToAddWithNonDistinctDomains.map { obj =>
      obj.copy(attributes = fixDomain(obj.attributes))
      // obj.attributes.transformField { case ("domain", JArray(vs)) => ("domain", JArray(vs.distinct)) }.to[SPAttributes].getOrElse(SPAttributes()))
    }

    //Operations------------------------------------------------------------------------------------
    lazy val opsToAdd = operations
    lazy val operationMap = opsToAdd.map(o => o.name -> o).toMap

    //ForbiddenExpressions--------------------------------------------------------------------------------------
    lazy val fesToAddWithNoVisableOperations = getIDablesFromSet(forbiddenExpressionSet, (n, as) => SOPSpec(name = n, sop = List(), attributes = as))
    lazy val fesToAdd = fesToAddWithNoVisableOperations.map { obj =>
      val mutexOperations = obj.attributes.findAs[List[String]]("mutexOperations").flatten
      obj.copy(sop = if (mutexOperations.isEmpty) List() else List(Arbitrary(mutexOperations.flatMap(o => operationMap.get(o)).map(o => OperationNode(o.id)))),
        attributes = play.api.libs.json.JsObject(obj.attributes.fields.filter(_._1 != "mutexOperations")))
    }

    //RobotMovements---------------------------------------------------------------------------------------------
    lazy val robotMovementsToAdd = getIDablesFromSet(robotMovementsSet, (n, as) => SPSpec(name = n, attributes = as))

    //Return--------------------------------------------------------------------------------------------
    val ids = varsToAdd ++ opsToAdd ++ fesToAdd ++ robotMovementsToAdd
    extendIDables(ids)
  }

}
