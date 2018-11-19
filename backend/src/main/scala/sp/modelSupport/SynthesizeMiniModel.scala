package sp.modelSupport

import sp.devicehandler._
import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.{ActionParser, PropositionParser, SOPLogic}
import sp.runners.APIOperationRunner

import scala.util.{Failure, Success, Try}
import sp.supremica._

trait SynthesizeMiniModel {
  def synthesizeModel(ids: List[IDAble], moduleName : String = "dummy"): (List[Operation], SPAttributes, Map[String, Int] => Option[Boolean]) = {

    // Extract from IDAbles

    // supremica cannot handle "." in strings... work around
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation]).
      filterNot(_.attributes.getAs[String]("isa") == Some("Ability")).map(o => (o, o.copy(name = o.name.replaceAll("\\.", "_"))))

    val vars = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing]).map(v => v.copy(name = v.name.replaceAll("\\.", "_")))

    val sopSpecs = ids.filter(_.isInstanceOf[SOPSpec]).map(_.asInstanceOf[SOPSpec])
    val spSpecs = ids.filter(_.isInstanceOf[SPSpec]).map(_.asInstanceOf[SPSpec])

    //Create Supremica Module and synthesize guards.
    val ptmw = MiniParseToModuleWrapper(moduleName, vars, ops.map(_._2), sopSpecs, spSpecs)
    val ptmwModule = {
      ptmw.addVariables()
      ptmw.saveToWMODFile("./testFiles/gitIgnore/")
      ptmw.addOperations() // add operations, with transitions and guards as events to the EFA for the later synthesis
      ptmw.saveToWMODFile("./testFiles/gitIgnore/")
      ptmw.addForbiddenExpressions()
      ptmw.saveToWMODFile("./testFiles/gitIgnore/")
      ptmw.saveToWMODFile("./testFiles/gitIgnore/raw/")
      ptmw.SupervisorAsBDD()
    }



    val optSupervisorGuards = ptmwModule.getSupervisorGuards.map(_.filter(og => !og._2.equals("1")))
    println("got guards from supremica: " + optSupervisorGuards)
    val updatedOps = optSupervisorGuards.map(newGuards => ops.map(_._2).flatMap(o => ptmw.addSynthGuards(o, newGuards))).getOrElse(List())

    lazy val synthesizedGuards = optSupervisorGuards.getOrElse(Map()).foldLeft(SPAttributes()) { case (acc, (event, guard)) =>
      acc merge SPAttributes("synthesizedGuards" -> SPAttributes(event -> guard))
    }
    val numStates = ptmwModule.nbrOfStates().getOrElse(-1l)
    lazy val nbrOfStates = SPAttributes("nbrOfStatesInSupervisor" -> numStates)
    println(s"Nbr of states in supervisor: ${numStates}")
    if (synthesizedGuards.value.nonEmpty) println(synthesizedGuards.pretty)

    ptmw.addSupervisorGuardsToFreshFlower(optSupervisorGuards)
    ptmw.saveToWMODFile("./testFiles/gitIgnore/")

    lazy val opsWithSynthesizedGuard = optSupervisorGuards.getOrElse(Map()).keys
    lazy val spAttributes = synthesizedGuards merge nbrOfStates merge SPAttributes("info" -> s"Model synthesized. ${opsWithSynthesizedGuard.size} operations are extended with a guard: ${opsWithSynthesizedGuard.mkString(", ")}") merge SPAttributes("moduleName" -> moduleName)

    val renameBackOps = updatedOps.flatMap{o =>
      ops.map(_._1).find(_.id == o.id).map(orig => o.copy(name = orig.name))
    }

    // TODO: refactor to only return the new guards...
    (renameBackOps, spAttributes, (x => ptmwModule.containsState(x)))
  }
}

// TODO: for now this is only for the transitions with the "ability runner" transition systems
import sp.virtualdevice.AbilityRunnerTransitions._

case class MiniParseToModuleWrapper(moduleName: String, varsIn: List[Thing], opsIn: List[Operation], sopSpec: List[SOPSpec], spSpec: List[SPSpec]) extends FlowerPopulater with Exporters with Algorithms with TextFilePrefix {

  val precondKind = "pre"
  val postcondKind = "post"

  // use inputs to rewrite our conditions
  val inputs = varsIn.filter(_.attributes.getAs[Boolean]("input").getOrElse(false)).map(_.id)

  var tempActions: List[Action] = List()


  def findAssignToSPVal(o: Operation, assignee: ID):Option[SPValue] = {
    val allActions = getConds(o.conditions, "pre").map(_.action).flatten
    val x = allActions.collect { case Action(`assignee`, ValueHolder(spval)) => spval }
    x.headOption
  }

  def updateGuard(o: Operation, guard: Proposition, inputs: List[ID]): Proposition = {
    guard match {
      case AND(xs) => AND(xs.map(x=>updateGuard(o, x, inputs)))
      case OR(xs) => OR(xs.map(x=>updateGuard(o, x, inputs)))
      case NOT(x) => NOT(updateGuard(o, x, inputs))
      case EQ(SVIDEval(id), SVIDEval(id2)) if inputs.contains(id) && inputs.contains(id2) => throw new Exception("assignment does not make sense: " + guard); AlwaysTrue

      //   // check if we know which value we expect
      // case EQ(SVIDEval(id), SVIDEval(id2)) if inputs.contains(id) && !inputs.contains(id2) =>
      //   val expects = findAssignToSPVal(o, id2)
      //   expects.map { spval =>
      //     println("ASSIGNING TO " + spval + " instead")
      //     tempActions = Action(id, ValueHolder(spval)) :: tempActions; AlwaysTrue
      //   }.getOrElse {
      //     tempActions = Action(id, ASSIGN(id2)) :: tempActions; AlwaysTrue
      //   }

      case EQ(SVIDEval(id), SVIDEval(id2)) if inputs.contains(id) && !inputs.contains(id2) => tempActions = Action(id, ASSIGN(id2)) :: tempActions; AlwaysTrue
      case EQ(SVIDEval(id), SVIDEval(id2)) if !inputs.contains(id) && inputs.contains(id2) => tempActions = Action(id2, ASSIGN(id)) :: tempActions; AlwaysTrue

      case EQ(SVIDEval(id), ValueHolder(x)) if inputs.contains(id) => tempActions = Action(id, ValueHolder(x)) :: tempActions; AlwaysTrue
      case EQ(ValueHolder(x), SVIDEval(id)) if inputs.contains(id) => tempActions = Action(id, ValueHolder(x)) :: tempActions; AlwaysTrue

        // todo, check for inequalities.... these cannot be handled
      case x => x
    }
  }

  //val ops = opsIn
  val ops = opsIn.map { o =>
    // only change "post"s
    val posts = o.conditions.filter(_.attributes.getAs[String]("kind").contains("post"))
    val notPosts = o.conditions.filterNot(_.attributes.getAs[String]("kind").contains("post"))
    val newPosts = posts.map {c =>
      tempActions = List()
      val ng = updateGuard(o, c.guard, inputs)
      val na = c.action ++ tempActions
      c.copy(guard = ng, action = na)
    }
    o.copy(conditions = notPosts ++ newPosts)
  }

  // ops.foreach(println)
  // println("============================================")
  // newOps.foreach(println)

  // throw new Exception("blah")

  // dont include both not enabled and enabled in synthesis. replace them with one "init"
  val init = "init"

  val opVars = ops.map{o =>
    // create operation state variable
    // TODO: only for abilityrunner...

    val domain = List(init, AbilityStates.starting, AbilityStates.executing, AbilityStates.finished).map(SPValue(_))
    val marked = Set(init).map(SPValue(_)) // TODO: think about if this is good

    Thing(o.name, SPAttributes("initialState" -> SPValue(init), "domain" -> domain, "marked" -> marked), id = o.id)
  }

  val vars = opVars ++ varsIn

  lazy val variableNameDomainMap = vars.flatMap(v => {
    v.attributes.getAs[List[SPValue]]("domain").map(d => v.name -> d)
  }).toMap

  lazy val mModule = SimpleModuleFactory(moduleName)

  def getConds(conds: List[Condition], kind: String)  =
    conds.filter(_.attributes.getAs[String]("kind").getOrElse("")==kind)

  private def addTransition(o: Operation, event: String, kind: String) = {
    val allGuards = getConds(o.conditions, kind).map(_.guard)
    val allActions = getConds(o.conditions, kind).map(_.action).flatten
    println("actions for " + o.name + " allActions" + allActions.toString)

    val combinedGuards = AND(allGuards)

    // This will add the event to an EFA. Before doing so, the guards and actions are converted to supremica syntax (The variable string values are replaced by corresponding numbers ( idle = "0" ..etc. .))
    addLeaf(event, propToSupremicaSyntax(combinedGuards),
      allActions.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))
  }

  def addOperations() = {
    ops.foreach { o =>

      //////////////////
      // val eventNotEnabledToEnabled = o.name+"notEnabledToEnabled"

      // val isNotEnabled = EQ(o.id, ValueHolder(SPValue(AbilityStates.notEnabled)))
      // val isNotEnabledWithPre = AND(isNotEnabled :: getConds(o.conditions, AbilityKinds.pre).map(_.guard))
      // val setEnabled = Action(o.id, ValueHolder(SPValue(AbilityStates.enabled)))
      // addEventIfNeededElseReturnExistingEvent(eventNotEnabledToEnabled, unControllable = true)
      // addLeaf(eventNotEnabledToEnabled, propToSupremicaSyntax(isNotEnabledWithPre), actionToSupremicaSyntax(setEnabled).get)

      //////////////////
      // val eventEnabledToNotEnabled = o.name+"enabledToNotEnabled"

      // val isEnabled = EQ(o.id, ValueHolder(SPValue(AbilityStates.enabled)))
      // val isEnabledWithNotPre = AND(List(isEnabled, NOT(AND(getConds(o.conditions, AbilityKinds.pre).map(_.guard)))))
      // val setNotEnabled = Action(o.id, ValueHolder(SPValue(AbilityStates.notEnabled)))
      // addEventIfNeededElseReturnExistingEvent(eventEnabledToNotEnabled, unControllable = true)
      // addLeaf(eventEnabledToNotEnabled, propToSupremicaSyntax(isEnabledWithNotPre), actionToSupremicaSyntax(setNotEnabled).get)

      //////////////////  THE ONLY ONE WE DISABLE! JUST USE THE OPERATION NAME TO MAP BACK TO GUARDS

      val isInit = EQ(o.id, ValueHolder(SPValue(init)))

      val started = getConds(o.conditions, AbilityKinds.started).map(_.guard)
      if(started.isEmpty || started.forall(_ == AlwaysTrue)) { // dont add the extra transition
        val eventEnabledToExec = o.name // +"enabledToStarting"
        val isEnabledWithPre = AND(List(isInit, AND(getConds(o.conditions, AbilityKinds.pre).map(_.guard))))
        val setExec = Action(o.id, ValueHolder(SPValue(AbilityStates.executing)))
        val setExecWithPre = setExec :: getConds(o.conditions, AbilityKinds.pre).map(_.action).flatten
        addEventIfNeededElseReturnExistingEvent(eventEnabledToExec, unControllable = false)
        addLeaf(eventEnabledToExec, propToSupremicaSyntax(isEnabledWithPre), setExecWithPre.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))
      } else {
        val eventEnabledToStarting = o.name // +"enabledToStarting"
        val isEnabledWithPre = AND(List(isInit, AND(getConds(o.conditions, AbilityKinds.pre).map(_.guard))))
        val setStarting = Action(o.id, ValueHolder(SPValue(AbilityStates.starting)))
        val setStartingWithPre = setStarting :: getConds(o.conditions, AbilityKinds.pre).map(_.action).flatten
        addEventIfNeededElseReturnExistingEvent(eventEnabledToStarting, unControllable = false)
        addLeaf(eventEnabledToStarting, propToSupremicaSyntax(isEnabledWithPre), setStartingWithPre.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))

        /////////////////
        val eventStartingToExec = o.name+"startingToExecuting"
        val isStarting = EQ(o.id, ValueHolder(SPValue(AbilityStates.starting)))
        val setExec = Action(o.id, ValueHolder(SPValue(AbilityStates.executing)))
        val isStartingWithStarting = AND(List(isStarting, AND(getConds(o.conditions, AbilityKinds.started).map(_.guard))))
        val setExecWithStarting = setExec :: getConds(o.conditions, AbilityKinds.started).map(_.action).flatten
        addEventIfNeededElseReturnExistingEvent(eventStartingToExec, unControllable = true)
        addLeaf(eventStartingToExec, propToSupremicaSyntax(isStartingWithStarting), setExecWithStarting.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))

      }


      /////////////////
      val setInit = Action(o.id, ValueHolder(SPValue(init)))
      val reset = getConds(o.conditions, AbilityKinds.reset).map(_.guard)
      if(reset.forall(_ == AlwaysTrue)) { // dont add the extra transition
        val eventExecToNotEnabled = o.name+"executingToNotEnabled"
        val isExecuting = EQ(o.id, ValueHolder(SPValue(AbilityStates.executing)))
        // val setNotEnabled = Action(o.id, ValueHolder(SPValue(AbilityStates.notEnabled)))
        val isExecWithPost = AND(List(isExecuting, AND(getConds(o.conditions, AbilityKinds.post).map(_.guard))))
        val setNotEnabledWithPost = setInit :: getConds(o.conditions, AbilityKinds.post).map(_.action).flatten
        addEventIfNeededElseReturnExistingEvent(eventExecToNotEnabled, unControllable = true)
        addLeaf(eventExecToNotEnabled, propToSupremicaSyntax(isExecWithPost), setNotEnabledWithPost.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))
      } else {
        ///////////////// original
        val eventExecToFinished = o.name+"executingToFinished"
        val isExecuting = EQ(o.id, ValueHolder(SPValue(AbilityStates.executing)))
        val setFinished = Action(o.id, ValueHolder(SPValue(AbilityStates.finished)))
        val isExecWithPost = AND(List(isExecuting, AND(getConds(o.conditions, AbilityKinds.post).map(_.guard))))
        val setFinishedWithPost = setFinished :: getConds(o.conditions, AbilityKinds.post).map(_.action).flatten
        addEventIfNeededElseReturnExistingEvent(eventExecToFinished, unControllable = true)
        addLeaf(eventExecToFinished, propToSupremicaSyntax(isExecWithPost), setFinishedWithPost.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))


        ////////////////
        val eventFinToNotEnabled = o.name+"finishedToNotEnabled"
        val isFinished = EQ(o.id, ValueHolder(SPValue(AbilityStates.finished)))
        val isFinishedWithReset = AND(List(isFinished, AND(getConds(o.conditions, AbilityKinds.reset).map(_.guard))))
        val setNotEnabledWithReset = setInit :: getConds(o.conditions, AbilityKinds.reset).map(_.action).flatten
        addEventIfNeededElseReturnExistingEvent(eventFinToNotEnabled, unControllable = true)
        addLeaf(eventFinToNotEnabled, propToSupremicaSyntax(isFinishedWithReset), setNotEnabledWithReset.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))
      }
    }
  }

  def addVariables() = {
    vars.foreach { v => for {
      domain <- variableNameDomainMap.get(v.name)
      init <- v.attributes.getAs[SPValue]("initialState")
      intInit <- getFromVariableDomain(v.name, init, "Problem with init") // get index value of the init variable
    } yield {
      // marked states need to be explicit in attribute "marked"
      val marked = v.attributes.getAs[List[SPValue]]("marked").map(_.flatMap(m=>getFromVariableDomain(v.name, m, "Problem with marking"))).getOrElse(List()).toSet

      println("adding variable: " + v.name + " with domain " + domain + " with init " + intInit + " with markings " + marked)

      addVariable(v.name, 0, domain.size - 1, intInit, marked)
      //Add variable values to module comment
      mModule.setComment(s"$getComment${TextFilePrefix.VARIABLE_PREFIX}${v.name} d${TextFilePrefix.COLON}${domain.mkString(",")}")

      // if variable is "input", add uncontrollable transitions between all values in domain
      // ... maybe too expensive?
      // val i = v.attributes.getAs[Boolean]("input").getOrElse(false)
      // if(i) {
      //   domain.foreach { s =>
      //     val index1 = domain.indexOf(s)
      //     val event = s"$UNCONTROLLABLE_PREFIX${v.name}to${index1}"
      //     val guard = v.attributes.getAs[Proposition]("activeWhen").map(c => propToSupremicaSyntax(c)).getOrElse("1")
      //     addEventIfNeededElseReturnExistingEvent(event, unControllable = true)
      //     // addLeaf(event, "1", s"${v.name}=${index1.toString}")
      //     addLeaf(event, guard, s"${v.name}=${index1.toString}")
      //   }
      // }
    }
    }
  }

  def addForbiddenExpressions() = {
    spSpec.foreach { s =>
      s.attributes.getAs[List[Proposition]]("forbiddenExpressions").foreach{ fes =>
        val xpr = fes.map(propToSupremicaSyntax).mkString("(", ")|(", ")").replaceAll("\\.", "_")
        println("adding forbidden expression: " + xpr + " (" + fes.toString + ")")
        addForbiddenExpression(forbiddenExpression = xpr, addSelfLoop = false, addInComment = true)
      }
    }

    // TODO: add support for SOPs
    // sopSpec.foreach { s =>
    //   s.sop.foreach {
    //     case a: Arbitrary if a.sop.forall(sopIsOneOpOrAStraightSeqOfOps) => addForbiddenExpressionWorkerForOneArbitrary(a)
    //     case s: Sequence if s.sop.forall { node => node.isInstanceOf[Arbitrary] && node.asInstanceOf[Arbitrary].sop.forall(sopIsOneOpOrAStraightSeqOfOps) } =>
    //       s.sop.foreach(a => addForbiddenExpressionWorkerForOneArbitrary(a.asInstanceOf[Arbitrary]))
    //     case _ => //do nothing
    //   }
    // }
  }

  // private def addForbiddenExpressionWorkerForOneArbitrary(a: Arbitrary): Unit = {
  //   lazy val operationIdMap = ops.map(o => o.id -> o).toMap
  //   def getOperationSeqsFromSop(sopNode: SOP): Seq[Seq[Operation]] = {
  //     sopNode.sop.map{
  //       case n: OperationNode => Seq(operationIdMap.get(n.operation)).flatten
  //       case s: Sequence => s.sop.flatMap(h => operationIdMap.get(h.asInstanceOf[OperationNode].operation)).toSeq
  //     }
  //   }
  //   def getExpressionOfExecutingSeq(seq: Seq[Operation]): String = {
  //     //For seq.head only expression for being executing
  //     //For seq.tail both expression for being enabled to start and executing
  //     def enabledToStart(o: Operation): String = directAttrValues(o, Set("preGuard")).mkString("(", ")&(", ")")
  //     def executing(o: Operation): String = directAttrValues(o, Set("postGuard")).mkString("(", ")&(", ")")
  //     lazy val headExp = Seq(executing(seq.head))
  //     lazy val tailExps = seq.tail.flatMap(o => Seq(enabledToStart(o), executing(o)))
  //     return (headExp ++ tailExps).mkString("(", ")|(", ")")
  //   }
  //   def addForbiddenExpressionForRemainingSeqExps(remainingSeqExps: Seq[String]): Unit = remainingSeqExps match {
  //     case exp +: exps if exps.nonEmpty =>
  //       exps.foreach { otherExp =>
  //         addForbiddenExpression(forbiddenExpression = stringPredicateToSupremicaSyntax(s"($exp)&($otherExp)"), addSelfLoop = false, addInComment = true)
  //       }
  //       addForbiddenExpressionForRemainingSeqExps(exps)
  //     case _ => //do nothing
  //   }
  //   //--This case starts here---
  //   //TODO Need to ignore operations that are the same from different sequences
  //   lazy val opSeqs = getOperationSeqsFromSop(a)
  //   lazy val seqsAsExp = opSeqs.map(getExpressionOfExecutingSeq)
  //   addForbiddenExpressionForRemainingSeqExps(seqsAsExp)
  // }

  private def sopIsOneOpOrAStraightSeqOfOps(sopToCheck: SOP): Boolean = sopToCheck match {
    case n: OperationNode => true
    case s: Sequence =>
      lazy val seq = sopToCheck.asInstanceOf[Sequence]
      seq.sop.forall(_.isInstanceOf[OperationNode])
    case _ => false
  }

  private def getFromVariableDomain(variable: String, value: SPValue, errorMsg: String): Option[Int] = {
    variableNameDomainMap.get(variable) match {
      case Some(domain) => domain.indexOf(value) match {
        case -1 => println(s"$errorMsg\nValue: $value is not in the domain of variable: $variable. Domain is: $domain. The result will not be correct!"); None.get
        case other => Some(other)
      }
      case _ => println(s"$errorMsg\nVariable: $variable is not defined. The result will not be correct!"); None.get

    }
  }

  // map back from indexes to domain values
  def varInDomain(id: ID, i: Int) = {
    for {
      v <- vars.find(_.id == id)
      domain <- v.attributes.getAs[List[SPValue]]("domain")
      value <- domain.lift(i)
    } yield {
      ValueHolder(value)
    }
  }

  // less than and more than are not defined for our spvalues... exhaust the domain instead
  def moreThan(id: ID, i: Int) = {
    for {
      v <- vars.find(_.id == id)
      domain <- v.attributes.getAs[List[SPValue]]("domain")
    } yield {
      val indexes = Range(i+1, domain.size).toList
      val eqs = indexes.map{ i =>
        val value = domain.lift(i).get
        EQ(SVIDEval(id), ValueHolder(value))
      }
      OR(eqs)
    }
  }

  def lessThan(id: ID, i: Int) = {
    for {
      v <- vars.find(_.id == id)
      domain <- v.attributes.getAs[List[SPValue]]("domain")
    } yield {
      val indexes = Range(0, i).toList
      val eqs = indexes.map{ i =>
        val value = domain.lift(i).get
        EQ(SVIDEval(id), ValueHolder(value))
      }
      OR(eqs)
    }
  }

  // Convert back into value of the domain array
  // Ugly!
  def sg(p: Proposition): Proposition = p match {
    case AND(ps) => AND(ps.map(sg))
    case OR(ps) => OR(ps.map(sg))
    case NOT(q) => NOT(sg(q))
    case EQ(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      EQ(q, v)
    case NEQ(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      NEQ(q, v)
    case GREQ(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      moreThan(id, i.value.toInt - 1).get
      // GREQ(q, v)
    case GR(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      moreThan(id, i.value.toInt).get
      // GR(q, v)
    case LEEQ(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      lessThan(id, i.value.toInt + 1).get
      // LEEQ(q, v)
    case LE(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      lessThan(id, i.value.toInt).get
      // LE(q, v)
    case x => x
  }

  def addSynthGuards(o: Operation, guardMap: Map[String, String]) = {
    val newCond = guardMap.get(o.name).flatMap { cond =>
      // cond needs parsing
      println("parsing supremica syntax: " + cond)

      PropositionParser(vars).parseStr(cond) match {
        case Right(p) => Some(p)
        case Left(fault) => println(s"PropositionParser failed for operation ${o.name} on guard: $cond. Failure message: $fault"); None
      }
    }.map(p => {
      println("parsed to: " + p.toString)
      val updp = sg(p)
      println("changed to: " + updp.toString)

      // KB: We should still use kind -> "pre", but filter on group
      o.copy(conditions = Condition(updp, List(), SPAttributes("kind" -> "pre", "group"->"synth")) :: o.conditions)
    })
    newCond
  }

  // def addSynthesizedGuardsToAttributes(o: Operation, optSynthesizedGuardMap: Option[Map[String, String]]) = {
  //   if (optSynthesizedGuardMap.isEmpty) o
  //   else {
  //     lazy val updatedAttribute = optSynthesizedGuardMap.get.get(o.name) match {
  //       case Some(guard) => o.attributes merge SPAttributes("preGuard" -> Set(guard))
  //       case _ => o.attributes
  //     }
  //     o.copy(attributes = updatedAttribute)
  //   }
  // }

  // def addSPConditionFromAttributes(o: Operation, optSynthesizedGuardMap: Option[Map[String, String]]): Operation = {
  //   if (optSynthesizedGuardMap.isEmpty) o
  //   else {
  //     def parseAttributesToPropositionCondition(op: Operation, idablesToParseFromString: List[IDAble]): Option[Operation] = {

  //       def getGuard(directGuardAttr: Set[String]) = {
  //         lazy val allGuards = directAttrValues(o, directGuardAttr)
  //         var guardAsString = if (allGuards.isEmpty) "" else allGuards.mkString("(", ")&(", ")")
  //         PropositionParser(idablesToParseFromString).parseStr(stringPredicateToSupremicaSyntax(guardAsString)) match {
  //           case Right(p) => Some(p)
  //           case Left(fault) => println(s"PropositionParser failed for operation ${op.name} on guard: $guardAsString. Failure message: $fault"); None
  //         }
  //       }

  //       def getAction(directActionAttr: Set[String]) = {
  //         val actionsAsStrings = directAttrValues(o, directActionAttr)
  //         actionsAsStrings.flatMap { action =>
  //           ActionParser(idablesToParseFromString).parseStr(stringActionToSupremicaSyntax(action)) match {
  //             case Right(a) => Some(a)
  //             case Left(fault) => println(s"ActionParser failed for operation ${op.name} on action: $action. Failure message: $fault"); None
  //           }
  //         }.toList
  //       }

  //       for {
  //         preGuard <- getGuard(Set("preGuard"))
  //         postGuard <- getGuard(Set("postGuard"))
  //       } yield {
  //         op.copy(conditions = List(Condition(preGuard, getAction(Set("preAction")), SPAttributes("kind" -> "precondition")),
  //           Condition(postGuard, getAction(Set("postAction")), SPAttributes("kind" -> "postcondition"))))
  //       }
  //     }
  //     parseAttributesToPropositionCondition(o, vars).getOrElse(o)
  //   }
  // }

  //To get correct syntax of guards and actions in Supremica
  //Variable values are changed to index in domain

  import sp.domain.logic.PropositionParser
  import sp.domain._

  private def stringActionToSupremicaSyntax(s: String) = ActionParser(vars).parseStr(s) match {
    case Right(a) => actionToSupremicaSyntax(a) match {
      case Some(r) => r
      case _ => a.toString
    }
    case other => other.toString
  }

  private def actionToSupremicaSyntax(a: Action) = {
    for {
      v <- vars.find(_.id==a.id).map(_.name)
    } yield {
      val valueStr = a.value match {
        case ValueHolder(spval) =>
          val index = getFromVariableDomain(v, spval, "Problem with action")
          val str = index.getOrElse("NONE")
          str
        case ASSIGN(id) =>
          val name = vars.find(_.id==id).get.name // die if we are not careful
                                                  // dont look up in domain for names
          name
        case other =>
          println(s"actionToSupremicaSyntax cannot handle: $other right now. sorry")
          SPValue(other.toString)
      }
      s"$v=$valueStr"
    }
  }

  private def stringPredicateToSupremicaSyntax(s: String) = PropositionParser().parseStr(s) match {
    case Right(p) => propToSupremicaSyntax(p)
    case other =>
      println("got: " + other.toString)
      other.toString
  }

  private def propToSupremicaSyntax(p: Proposition): String = p match {
    case AND(ps) => ps.map(propToSupremicaSyntax).mkString("(", ")&(", ")")
    case OR(ps) => ps.map(propToSupremicaSyntax).mkString("(", ")|(", ")")
    case NOT(q) => s"!${propToSupremicaSyntax(q)}"
    case EQ(l, r) => leftRight(l, "==", r)
    case NEQ(l, r) => leftRight(l, "!=", r)
    case GREQ(l, r) => leftRight(l, ">=", r)
    case GR(l, r) => leftRight(l, ">", r)
    case LEEQ(l, r) => leftRight(l, "<=", r)
    case LE(l, r) => leftRight(l, "<", r)
    case AlwaysTrue => "1"
    case AlwaysFalse => "0"
    case other =>
      println(s"propToSupremicaSyntax cannot handle: $other right now. sorry")
      other.toString
  }

  private def leftRight(l: StateEvaluator, operator: String, r: StateEvaluator) = {
    val left = l match {
      case SVIDEval(id) =>
        val name = vars.find(_.id==id).get.name // die if we are not careful
                                                     // dont look up in domain for names
        name
      case ValueHolder(spval) =>
        println("problem with guard, value holder " + spval + " on left side")
        "none"
    }

    val right = r match {
      case SVIDEval(id) =>
        val rightVar = vars.find(_.id==id).get.name // die if we are not careful
                                                    // dont look up in domain for names
        rightVar
      case ValueHolder(spval) =>
        getFromVariableDomain(left, spval, "Problem with guard").map(_.toString).getOrElse("NONE")
    }

    s"$left$operator$right"
  }

  private def isInt(s: String): Boolean = {
    try {
      s.toInt
      true
    } catch {
      case e: Exception => false
    }
  }

}
