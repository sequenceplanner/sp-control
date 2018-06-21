package sp.modelSupport

import sp.abilityhandler.APIAbilityHandler
import sp.abilityhandler.APIAbilityHandler.Ability
import sp.devicehandler._
import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.runners.APIOperationRunner
import scala.util.{Try,Success, Failure}

import sp.supremicaStuff.base._

object VariableKind {
  def fromString(s: String): Option[VariableKind] =
    List(ReadOnly, WriteOnly).find(_.toString == s)
}
sealed trait VariableKind
case object ReadOnly extends VariableKind
case object WriteOnly extends VariableKind

case class cond(kind: String, guard: String, actions: String*)
sealed trait ModelElement
case class Tdv(name: String, driverName: String, driverIdentifier: String, kind: VariableKind = ReadOnly) extends ModelElement
case class Tv(name: String, initState: SPValue, domain: List[SPValue]) extends ModelElement
case class Ta(name: String, parameters: List[String], pre:cond, running:cond, post:cond, reset:cond=cond("reset", "true")) extends ModelElement
case class To(name: String, ab: String, conds: List[cond]) extends ModelElement
case class Trunner(name: String, initState: Map[String, SPValue] = Map(), ops: List[String] = List()) extends ModelElement
case class Tresource(name: String, dvs: List[String] = List()) extends ModelElement
case class Tdriver(name: String, driverType: String, setup: SPAttributes = SPAttributes()) extends ModelElement
case class Tsubmodel(name: String, model: List[ModelElement]) extends ModelElement
case class Tx(name: String, exprs: List[String]) extends ModelElement
case class TpostBuildHook(func: List[IDAble] => List[IDAble]) extends ModelElement




trait ModelDSL extends BuildModel with SynthesizeModel {
  def buildModel(name: String = "") = {
    val idables = build(name, mes)

    val afterSynth = Try[List[IDAble]] {
      val (updOps,_,_) = synthesizeModel(idables)
      idables.filterNot(i=>updOps.exists(_.id==i.id))++updOps
    } match {
      case Success(ids) =>
        println("Synthesis successful")
        ids
      case Failure(t) =>
        println("Synthesis failed: " + t.getMessage)
        idables
    }

    val postHooks = mes.collect{ case bh: TpostBuildHook => bh }
    postHooks.foldLeft(afterSynth){case (idables, hook) => hook.func(idables)}
  }

  def c(kind: String, guard: String, actions: String*) = cond(kind, guard, actions:_*)
  var mes: List[ModelElement] = List()

  def dv(name: String, driverName: String, driverIdentifier: String, kind: VariableKind = ReadOnly) = mes :+= Tdv(name, driverName, driverIdentifier, kind)
  def v(name: String, initState: SPValue, domain: List[SPValue]) = mes :+= Tv(name, initState, domain)
  def v(name: String, initState: SPValue, domain: List[SPValue],
    driverName: String, driverIdentifier: String, kind: VariableKind = ReadOnly) = {
    dv(name, driverName, driverIdentifier, kind)
    mes :+= Tv(name, initState, domain)
  }
  def a(name: String, parameters: List[String], pre:cond, running:cond, post:cond, reset:cond=cond("reset", "true")) = mes :+= Ta(name, parameters, pre, running, post, reset)
  def o(name: String, ab: String="")(conds: cond*) = mes :+= To(name, ab, conds.toList)
  def x(name: String, expr: String) = mes :+= Tx(name, List(expr))
  def x(name: String, exprs: List[String]) = mes :+= Tx(name, exprs)

  def runner(name: String, initState: Map[String, SPValue] = Map(), ops: List[String] = List()) = mes :+= Trunner(name, initState, ops)
  def resource(name: String, dvs: List[String] = List()) = mes :+= Tresource(name, dvs)
  def driver(name: String, driverType: String, setup: SPAttributes = SPAttributes()) = mes :+= Tdriver(name, driverType, setup)
  def use(name: String, other: ModelDSL) = mes :+= Tsubmodel(name, other.mes)

  def addPostBuildHook(func: List[IDAble] => List[IDAble]) = mes :+= TpostBuildHook(func)
}

trait BuildModel {
  def driverMapper(drivers: List[VD.Driver], things: List[IDAble]): List[VD.OneToOneMapper] = things.flatMap { v =>
    for {
      driverName <- v.attributes.getAs[String]("driverName")
      driverIdentifier <- v.attributes.getAs[String]("driverIdentifier")
      driver <- drivers.find(_.name == driverName)
    } yield {
      VD.OneToOneMapper(v.id, driver.id, driverIdentifier)
    }
  }

  def parse(c: cond)(idables: List[IDAble]) = {
    val g = if (c.guard.toLowerCase == "true") Some(AlwaysTrue)
    else if (c.guard.toLowerCase == "false") Some(AlwaysFalse)
    else PropositionParser(idables).parseStr(c.guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $c.guard: $err"); None
    }
    val xs = c.actions.flatMap { action =>
      ActionParser(idables).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }
    }
    Condition(g.get, xs.toList, attributes = SPAttributes("kind"->c.kind))
  }

  case class tempR(name: String, ids: Set[ID], attr: SPAttributes)

  def build(name: String, mes: List[ModelElement]): List[IDAble] = {
    // collect all things and dthings per submodel
    val toplevel = Tsubmodel(name, mes)

    // recursively build submodels
    def buildM(prefix: String, m: Tsubmodel, dvTovMap: Map[ID,ID]=Map(), opAbMap: Map[ID,ID]=Map(), resources: List[tempR] = List()):
        (List[IDAble], Map[ID,ID], Map[ID,ID], List[tempR]) = {
      def nn(n: String) = if(prefix.isEmpty) n else prefix + "." + n
      def unnn(n: String) = if(prefix.isEmpty) n else n.stripPrefix(prefix+".")

      val submodels = m.model.collect { case t: Tsubmodel => t }
      val x = submodels.map(m=>buildM(nn(m.name),m,dvTovMap,opAbMap,resources))
      val deeperThings = x.flatten{_._1}
      val upddvTovMap_ = dvTovMap ++ x.flatten{_._2}.toMap
      val updopAbMap_ = opAbMap ++ x.flatten{_._3}.toMap
      val updResources_ = resources ++ x.flatten{_._4}

      // now we have all the variables "below" this level

      val vs = m.model.collect{
        case Tv(name: String, initState, domain) =>
          Thing(nn(name), SPAttributes("init" -> initState, "domain" -> domain))
      }
      val dvs = m.model.collect {
        case Tdv(name, driverName, driverIdentifier, kind) =>
          Thing(nn(name), SPAttributes("driverName" -> driverName, "driverIdentifier" -> driverIdentifier, "variableKind" -> kind.toString))
      }

      val dvsDeeper = deeperThings.collect{
        case t @ Thing(_, attr, _) if attr.get("driverName").isDefined => t
      }

      // dont create them by default anylonger...
      // by default create v:s for dv:s if not already defined
      // val newVs = dvs.filterNot(dv=>vs.exists(_.name == dv.name)).map(dv=>Thing(dv.name))
      val updVs = vs // ++ newVs
      val upddvTovMap: Map[ID,ID] = upddvTovMap_ ++ updVs.flatMap { v =>
        val allD = dvs ++ dvsDeeper
        allD.find(_.name == v.name).map(dv=>v.id->dv.id)
      }.toMap

      // we can now add abilities and operations matching to the names we have
      val abParseHelpers = dvs ++ dvs.map(dv=> dv.copy(name = unnn(dv.name)))
      val abs = m.model.collect {
        case Ta(name, params, pre, running, post, reset) =>
          val precond = parse(pre)(abParseHelpers)
          val runningcond = parse(running)(abParseHelpers)
          val postcond = parse(post)(abParseHelpers)
          val resetcond = parse(reset)(abParseHelpers)

          // find parameter ids. TODO: for now only search the same level...
          val p = updVs.filter(v=>params.contains(unnn(v.name))).map(_.id)

          val a = Ability(nn(name), ID.newID, precond, runningcond, postcond, resetcond, p)
          APIAbilityHandler.abilityToOperation(a)
      }

      val deeperVs = deeperThings.collect { case t: Thing if t.attributes.keys.contains("domain") =>  t}
      val opParseHelpersDeep = deeperVs ++ deeperVs.map(v=> v.copy(name = unnn(v.name)))
      val opParseHelpers = updVs ++ updVs.map(v=> v.copy(name = unnn(v.name))) ++ opParseHelpersDeep
      println("parseHelper: " + opParseHelpers.map(_.name).mkString(","))
      val searchAbs = deeperThings.collect { case o: Operation => APIAbilityHandler.operationToAbility(o) }.flatten
      val opsAndMapping = m.model.collect {
        case item: To =>
          val defaultPre = if (item.conds.exists(_.kind == "pre")) None else Some(Condition(AlwaysTrue, List(), attributes = SPAttributes("kind"->"pre")))
          val defautltPost = if (item.conds.exists(_.kind == "post")) None else Some(Condition(AlwaysFalse, List(), attributes = SPAttributes("kind"->"post")))
          val defautltReset = if (item.conds.exists(_.kind == "reset")) None else Some(Condition(AlwaysFalse, List(), attributes = SPAttributes("kind"->"reset")))
          val theCondition: List[Condition] = item.conds.toList.map(x =>
            parse(x)(opParseHelpers)
          ) ++ defaultPre ++ defautltPost ++ defautltReset

          val op = Operation(nn(item.name), theCondition)
          val mapping = if(item.ab.nonEmpty) {
            // hard coded mapping
            val a = searchAbs.find(a=>unnn(a.name)==item.ab) // break if we mis-spell
            if(a.isEmpty) { println("no such ability: " + item.ab) }
            Some(op.id->a.get.id)
          } else {
            // find by operation name, must be on the same "level"
            abs.find(_.name==op.name).map(a=>op.id->a.id)
          }
          (op, mapping)
      }

      val ops = opsAndMapping.map{_._1}
      val updOpAbMap = updopAbMap_ ++ opsAndMapping.flatMap{_._2}.toMap

      val forbidden = m.model.collect { case x: Tx =>
        // parse expression
        val props = x.exprs.flatMap{x=>
          PropositionParser(opParseHelpers).parseStr(x) match {
            case Right(p) => Some(p)
            case Left(err) => println(s"Parsing failed on forbidden expression: $x: $err"); None
          }
        }
        SPSpec(x.name, SPAttributes("forbiddenExpressions" -> props))
      }

      val levelThings = updVs ++ dvs ++ abs ++ ops ++ forbidden

      val updResources = updResources_ ++ m.model.collect {
        case Tresource(name: String, dts: List[String]) =>
          val n = nn(name)
          println("adding resource: " + n)
          val ids = if(dts.isEmpty) {
            // take all dvs on this level
            // TODO: perhaps collect dvs on all sublevels if they dont have a resource
            dvs
          } else {
            // take specified names
            val check = (deeperThings ++ updVs ++ dvs).collect {
              case t: Thing if t.attributes.keys.contains("driverName") && // TODO, maybe add an isa for dvs
                  (dts.contains(t.name) || dts.contains(unnn(t.name))) => t
            }
            assert(check.length == dts.length) // crash if we make a mistake
            check
          }
          tempR(n, ids.map(_.id).toSet, SPAttributes()) // todo: include attributes
      }

      println("deeper short name: ")
      deeperThings.map(t=>unnn(t.name)).foreach {println}
      println("deeper full name")
      deeperThings.map(t=>(t.name)).foreach {println}

      println("level short name: ")
      levelThings.map(t=>unnn(t.name)).foreach {println}
      println("level full name")
      levelThings.map(t=>(t.name)).foreach {println}

      val things = deeperThings ++ levelThings
      (things, upddvTovMap, updOpAbMap, updResources)
    }

    val x = buildM(toplevel.name, toplevel)

    // driver names are global. collect the drivers first, then
    // collect all identifiers from the model
    def getDrivers(l: List[ModelElement]): List[Tdriver] = {
      l.collect {
        case t:Tdriver => List(t)
        case s:Tsubmodel => getDrivers(s.model)
      }.flatten
    }
    val drivers = getDrivers(toplevel.model).map { case Tdriver(name, driverType, setup) =>
      val did = ID.newID

      val thingsToSearch = x._1.collect { case t: Thing => t }
      val driverIdentifiers = thingsToSearch.flatMap { v => for {
        driverName <- v.attributes.getAs[String]("driverName") if driverName == name
        driverIdentifier <- v.attributes.getAs[String]("driverIdentifier")
      } yield {
        driverIdentifier
      }}

      val driverSetup = setup merge SPAttributes("identifiers" -> driverIdentifiers)
      VD.Driver(name, did, driverType, driverSetup)
    }
    val driverThings = drivers.map(d => VD.driverToThing(d))

    println("DRIVERS: " + drivers)

    // now that we have our driver ids, we can set up our resources
    val resources = x._4
    val resourceThings = x._4.map { r =>
      val things = x._1.collect { case t: Thing if r.ids.contains(t.id) => t }
      VD.resourceToThing(
        VD.Resource(r.name, ID.newID, r.ids, driverMapper(drivers, things), SPAttributes()))
    }

    println("RESOURCES: " + resources)

    // TODO: fix this. for now assume that runner is on the top level
    // only and take the entire model
    val r = toplevel.model.collect { case r:Trunner => r }.head
    val dvTovMap = x._2
    val opAbMap = x._3
    val mappedOps = x._1.collect { case o: Operation if opAbMap.contains(o.id) => o }.toSet
    val init = x._1.collect { case t: Thing => t }.flatMap{t => t.attributes.get("init").map(v=>t.id->v) }.toMap
    // ability -> params map
    val absWithParams = x._1.collect { case o: Operation if o.attributes.getAs[String]("isa") == Some("Ability") &&
      o.attributes.getAs[List[ID]]("parameters").getOrElse(List()) != List() => o }
    val paramMap = absWithParams.flatMap {a => a.attributes.getAs[List[ID]]("parameters").map(l => a.id -> l.toSet) }.toMap

    val runner = APIOperationRunner.Setup(r.name, ID.newID, mappedOps,
      opAbMap, init, dvTovMap, paramMap) //TODO add parameters

    val runnerThing = APIOperationRunner.runnerSetupToThing(runner)

    x._1 ++ driverThings ++ resourceThings ++ List(runnerThing)
  }
}


trait SynthesizeModel {
  def synthesizeModel(ids: List[IDAble], moduleName : String = "dummy"): (List[Operation], SPAttributes, Map[String, Int] => Option[Boolean]) = {

    // Extract from IDAbles

    // supremica cannot handle "." in strings... work around
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation]).
      filterNot(_.attributes.getAs[String]("isa") == Some("Ability")).map(o => (o, o.copy(name = o.name.replaceAll("\\.", "_"))))

    val vars = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing]).map(v => v.copy(name = v.name.replaceAll("\\.", "_")))

    val sopSpecs = ids.filter(_.isInstanceOf[SOPSpec]).map(_.asInstanceOf[SOPSpec])
    val spSpecs = ids.filter(_.isInstanceOf[SPSpec]).map(_.asInstanceOf[SPSpec])

    //Create Supremica Module and synthesize guards.
    val ptmw = ParseToModuleWrapper(moduleName, vars, ops.map(_._2), sopSpecs, spSpecs)
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
    lazy val nbrOfStates = SPAttributes("nbrOfStatesInSupervisor" -> ptmwModule.nbrOfStates())
    println(s"Nbr of states in supervisor: ${nbrOfStates.getAs[String]("nbrOfStatesInSupervisor").getOrElse("-")}")
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


case class ParseToModuleWrapper(moduleName: String, vars: List[Thing], ops: List[Operation], sopSpec: List[SOPSpec], spSpec: List[SPSpec]) extends FlowerPopulater with Exporters with Algorithms with TextFilePrefix {

  val precondKind = "pre"
  val postcondKind = "post"
  val synthKind = "synthPre"

  lazy val variableNameDomainMap = vars.flatMap(v => {
    v.attributes.getAs[List[SPValue]]("domain").map(d => v.name -> d)
  }).toMap

  lazy val mModule = SimpleModuleFactory(moduleName)

  def getConds(conds: List[Condition], kind: String)  =
    conds.filter(_.attributes.getAs[String]("kind").getOrElse("")==kind)

  private def addTransition(o: Operation, event: String, kind: String) = {
    val allGuards = getConds(o.conditions, kind).map(_.guard)
    val allActions = getConds(o.conditions, kind).map(_.action).flatten

    val combinedGuards = AND(allGuards)

    // This will add the event to an EFA. Before doing so, the guards and actions are converted to supremica syntax (The variable string values are replaced by corresponding numbers ( idle = "0" ..etc. .))
    addLeaf(event, propToSupremicaSyntax(combinedGuards),
      allActions.flatMap(a => actionToSupremicaSyntax(a)).mkString("; "))
  }

  def addOperations() = {
    ops.foreach { o =>
      //pre
      val startEvent = o.name
      addEventIfNeededElseReturnExistingEvent(startEvent, unControllable = false)
      addTransition(o, startEvent, precondKind)

      //post
      val compEvent = s"$UNCONTROLLABLE_PREFIX$startEvent"

      addEventIfNeededElseReturnExistingEvent(compEvent, unControllable = true)
      addTransition(o, compEvent, postcondKind)

      //Add operation events  to module comment
      mModule.setComment(s"$getComment$OPERATION_PREFIX${o.name} $TRANSITION_PREFIX$startEvent,$compEvent")
    }
  }

  def addVariables() = {
    vars.foreach { v => for {
      domain <- variableNameDomainMap.get(v.name)
      init <- v.attributes.getAs[SPValue]("init")
      intInit <- getFromVariableDomain(v.name, init, "Problem with init") // get index value of the init variable
    } yield {
      // Todo: Here, the marked state is the same as the initial state, and no more than 1 marked state is allowed. This needs investigating..
      addVariable(v.name, 0, domain.size - 1, intInit, Set(intInit))
      //Add variable values to module comment
      mModule.setComment(s"$getComment${TextFilePrefix.VARIABLE_PREFIX}${v.name} d${TextFilePrefix.COLON}${domain.mkString(",")}")
    }
    }
  }

  def addForbiddenExpressions() = {
    spSpec.foreach { s =>
      s.attributes.getAs[List[Proposition]]("forbiddenExpressions").foreach{ fes =>
        val xpr = fes.map(propToSupremicaSyntax).mkString("(", ")|(", ")").replaceAll("\\.", "_")
        println("adding forbidden expression: " + xpr)
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
        case -1 => println(s"$errorMsg\nValue: $value is not in the domain of variable: $variable. The result will not be correct!"); None.get
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
      GREQ(q, v)
    case GR(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      GR(q, v)
    case LEEQ(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      LEEQ(q, v)
    case LE(q@SVIDEval(id), ValueHolder(i:play.api.libs.json.JsNumber)) =>
      val v = varInDomain(id, i.value.toInt).get // want to know if we fail
      LE(q, v)
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

      o.copy(conditions = Condition(updp, List(), SPAttributes("kind" -> synthKind)) :: o.conditions)
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
    val varsIdMap = vars.map(v => v.id -> v.name).toMap
    val value = a.value match {
      case ValueHolder(play.api.libs.json.JsString(v)) => v
      case ValueHolder(play.api.libs.json.JsNumber(v)) => v.toString()
      case other =>
        println(s"actionToSupremicaSyntax cannot handle: $other right now. sorry")
        other.toString
    }
    for {
      variable <- varsIdMap.get(a.id)
    } yield {
      s"$variable=${if (isInt(value)) value else getFromVariableDomain(variable, value, "Problem with action").getOrElse("NONE")}"
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
    val left = stateEvalToSupremicaSyntax(l)
    val right = stateEvalToSupremicaSyntax(r)
    s"$left$operator${if (isInt(right)) right else getFromVariableDomain(left, right, "Problem with guard").getOrElse("NONE")}"
  }

  private def stateEvalToSupremicaSyntax(se: StateEvaluator): String = se match {
    case ValueHolder(play.api.libs.json.JsTrue) => "1"
    case ValueHolder(play.api.libs.json.JsFalse) => "0"
    case ValueHolder(play.api.libs.json.JsString(v)) => v
    case ValueHolder(play.api.libs.json.JsNumber(v)) => v.toString()
    case SVIDEval(id) => vars.find(_.id==id).get.name // die if we are not careful
    case other =>
      println(s"stateEvalToSupremicaSyntax cannot handle: $other right now. sorry")
      other.toString
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
