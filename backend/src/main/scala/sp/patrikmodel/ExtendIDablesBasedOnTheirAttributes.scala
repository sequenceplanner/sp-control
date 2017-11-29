package sp.patrikmodel

import akka.actor._
import sp.domain._
import sp.domain.Logic._

/**
 * Extends IDables
 * Operations:
 * "carrierTrans" attributes are extended to get a value keys "atStart", "atExecute", "atComplete" if no exists.
 * E.g. If "atStart":"gripper" and no values are given for "atExecute" and "atComplete" then "atExecute":"partlyGripper" and "atComplete":"empty"
 * SPSpec:
 * "staticRobotPoses" attributes are extended to generate new (transport) operations
 * E.g. {"atHome":{{"to":"atFixture"},{"to":"atTable","simop":"1,202"}}} will generate two operations:
 * "atHomeToAtFixture" and "atHomeToAtTable" (with an attribute "simop":"1,202")
 * Variables:
 * Non-existing variables referenced in attributes of operations are created.
 * The domains for new and old variables are extended based on values given in attributes of operations.
 * Variables without an attribute key "idleValue" that contains the value "empty" in its domain are extend with the attribute: "idleValue":"empty"
 *
 * TODO: Extend based on product SOPs
 */

trait ExtendIDables {
  def extendIDables(ids: List[IDAble]) = {
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])
    val vars = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val sopSpecs = ids.filter(_.isInstanceOf[SOPSpec]).map(_.asInstanceOf[SOPSpec])
    val spSpecs = ids.filter(_.isInstanceOf[SPSpec]).map(_.asInstanceOf[SPSpec])

    println("\n vars    extend   "    + vars + "\n")
    //Extend Operations and Variables (TODO extend based on product sequences)
    val eiw = ExtendIDablesWrapper(ops, vars, sopSpecs, spSpecs)
    val updatedIDables = {
      eiw.extend()
      eiw.extendedIDables()
    }

    updatedIDables
  }
}

case class TransformationPatternInAttributes(atStart: Option[String], atExecute: Option[String], atComplete: Option[String]) {
  def partlyAtStart() = partly(atStart)
  def partlyAtComplete() = partly(atComplete)
  def betweenStartAndComplete() = for {
    start <- atStart
    complete <- atComplete
  } yield {
      s"${start}To${complete.capitalize}"
    }
  private def partly(optValue: Option[String]) = optValue.map(value => s"partly${value.capitalize}")
  def valuesForDomain() = Seq(atStart, atExecute, atComplete).flatten
}



case class StaticRobotPosesInfoInAttributes(to: String, simop: Option[String])

private case class ExtendIDablesWrapper(var ops: List[Operation], var vars: List[Thing], var sopSpecs: List[SOPSpec], var spSpecs: List[SPSpec]) {
  import play.api.libs.json._
  implicit val fT: JSFormat[TransformationPatternInAttributes] = Json.format[TransformationPatternInAttributes]
  implicit val sT: JSFormat[StaticRobotPosesInfoInAttributes] = Json.format[StaticRobotPosesInfoInAttributes]

  def extendedIDables() = ops ++ vars ++ sopSpecs

  def extend() = {
    extendOpsWithValuesForAttributeCarrierTrans()
    addTransportOperations()
    addVariablesAndUpdateDomainsForExistingVariables()
  }

  //Extend ops with values for attribute: carrierTrans
  def extendOpsWithValuesForAttributeCarrierTrans() = {
    ops = ops.map { o =>
      lazy val attributeKey = "carrierTrans"

      lazy val updatedAttr = o.attributes.getAs[SPAttributes](attributeKey).map {
        _.fields.map { case (key, toTpia) =>
          lazy val tpia = toTpia.to[TransformationPatternInAttributes].get
          SPAttributes(key -> SPAttributes("atStart" -> SPValue(tpia.atStart.getOrElse("empty")),
            "atExecute" -> (if (tpia.atExecute.isDefined) tpia.atExecute else if (tpia.atComplete.isDefined && tpia.atStart.isDefined) tpia.betweenStartAndComplete() else if (tpia.atComplete.isDefined) tpia.partlyAtComplete() else tpia.partlyAtStart()),
            "atComplete" -> SPValue(tpia.atComplete.getOrElse("empty"))))
        }
      }.map(_.foldLeft(SPAttributes()) { case (acc, attr) => acc merge attr })

      if(updatedAttr.nonEmpty)
        o.copy(attributes = (o.attributes merge SPAttributes(attributeKey -> SPValue(updatedAttr.get))))
      else o
//        transformField { case (`attributeKey`, _) => (attributeKey, SPValue(updatedAttr)) }).to[SPAttributes].getOrElse(SPAttributes()))
    }
  }

  //Robot movements
  def addTransportOperations() = {
    lazy val operationMap = ops.map(o => o.name -> o).toMap
    spSpecs.foreach { obj =>
      lazy val transportOps = for {
        robotNamePrefix <- obj.attributes.getAs[String]("robotNamePrefix")
        robotName <- obj.attributes.getAs[String]("robotName")
        robotNameSuffix <- obj.attributes.getAs[String]("robotNameSuffix")
        staticRobotPoses <- obj.attributes.getAs[SPAttributes]("staticRobotPoses")
      } yield {
          staticRobotPoses.fields.flatMap { case (from, toSrpiia) =>
            toSrpiia.to[List[StaticRobotPosesInfoInAttributes]].toOption.map {
              _.map { srpiia =>
                lazy val inBetweenValue = s"${from}To${srpiia.to.capitalize}"
                lazy val robot_pos = s"$robotNamePrefix$robotName$robotNameSuffix"
                lazy val operationName = s"${inBetweenValue}_$robotName"
                lazy val attr = SPAttributes("resourceTrans" -> SPAttributes(robot_pos -> SPAttributes("atStart" -> from, "atExecute" -> inBetweenValue, "atComplete" -> srpiia.to)),
                  "simop" -> srpiia.simop)

                operationMap.get(operationName) match {
                  case Some(tOp) => tOp.copy(attributes = tOp.attributes merge attr)
                  case _ => Operation(name = s"${inBetweenValue}_$robotName", attributes = attr.addTimeStamp)
                }
              }
            }
          }
        }
      transportOps.foreach(tOps => ops = ops ++ tOps.flatten)
    }
  }

  def fixDomain(attr: SPAttributes): play.api.libs.json.JsObject = {
    play.api.libs.json.JsObject(attr.fields.map {
      case ("domain", v) =>
        val nd = v.to[List[String]].getOrElse(List()).distinct
        ("domain", play.api.libs.json.JsArray(nd.map(play.api.libs.json.JsString(_))))
      case (k, v) =>
        if(v.to[play.api.libs.json.JsObject].toOption.isEmpty)
          (k, v)
        else
          (k, fixDomain(v.to[play.api.libs.json.JsObject].toOption.get).to[play.api.libs.json.JsValue].get)
    })
  }


  //Add variables and update domains. Set default init/marked values.
  def addVariablesAndUpdateDomainsForExistingVariables() = {
    lazy val variableMap = vars.map(o => o.name -> o).toMap

    lazy val variableValueFromOpsMapOld = ops.flatMap { o =>
      Set("carrierTrans", "resourceTrans").flatMap { transType =>
        o.attributes.getAs[SPAttributes](transType).map {
          _.fields.flatMap { case (variable, toTpia) =>
            toTpia.to[TransformationPatternInAttributes].toOption.map { tpia =>
              variable -> SPAttributes("stateVariable" -> SPAttributes("domain" -> tpia.valuesForDomain()))
            }
          }
        }
      }
    }.flatten.groupBy(kv => kv._1).map(kv => kv._1 -> kv._2.unzip._2.foldLeft(SPAttributes()) { case (acc, attr) => acc merge attr }) // This only contains the last value from the mapping


    // New try
    lazy val variableValueFromOpsMap= ops.flatMap { o =>
      Set("carrierTrans", "resourceTrans").flatMap { transType =>
        o.attributes.getAs[SPAttributes](transType).map {
          _.fields.flatMap { case (variable, toTpia) =>
            println("\n operation resourceTrans stuff\n" + o.name + "  " + variable + "  " + toTpia) // This displays all values that should be there
            toTpia.to[TransformationPatternInAttributes].toOption.map { tpia =>
              variable -> SPAttributes("stateVariable" -> SPAttributes("domain" -> tpia.valuesForDomain())) // <-- This is probably the problem!!
            }
          }
        }
      }
    }.flatten.groupBy(kv => kv._1).map(kv => kv._1 -> kv._2.unzip._2  .foldLeft (SPAttributes()) { case (acc, attr) => {
     val accDomain =acc.getAs[SPAttributes]("stateVariable").getOrElse(SPAttributes()).getAs[Seq[String]]("domain").getOrElse(Seq())
     val attrDomain =attr.getAs[SPAttributes]("stateVariable").getOrElse(SPAttributes()).getAs[Seq[String]]("domain").getOrElse(Seq())
     val newAttr = SPAttributes( "stateVariable" -> SPAttributes(
       "domain" -> (accDomain ++attrDomain).distinct
       ))
      newAttr} } ) // This merges the values correctly, but a bit ugly
    println("\n cropped   \n" + variableValueFromOpsMap)

    vars = variableValueFromOpsMap.map {
      case (variable, attr) => variableMap.get(variable) match {
        case Some(thing) => thing.copy(attributes = thing.attributes merge attr)
        case _ => Thing(name = variable, attributes = attr.addTimeStamp)
      }
    }.map {
      obj => obj.copy(attributes = fixDomain(obj.attributes))
    }.map {
      obj => obj.attributes.getAs[String]("idleValue") match {
        case Some(_) => obj //Do nothing
        case _ => obj.attributes.findAs[List[String]]("domain").headOption match {
          case Some(domainList) => if (domainList.contains("empty")) obj.copy(attributes = obj.attributes merge SPAttributes("idleValue" -> "empty")) else obj
          case _ => obj //Do nothing
        }
      }
    }.toList
  }
}
