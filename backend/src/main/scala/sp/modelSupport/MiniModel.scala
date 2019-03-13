package sp.modelSupport

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import sp.domain.Logic._
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser, SOPLogic}

import sp.runners._
import sp.runners.Shared._
import sp.runners.API._


object SOPLogic2 extends SOPLogics2

trait SOPLogics2 {
  val kind = "pre"
  val init = sp.runners.AbilityRunnerTransitions.AbilityStates.enabled
  val executing = sp.runners.AbilityRunnerTransitions.AbilityStates.executing
  val finished = sp.runners.AbilityRunnerTransitions.AbilityStates.finished

  implicit def operationToSOP(o: Operation): SOP = OperationNode(o.id)
  implicit def operationIDToSOP(o: ID): SOP = OperationNode(o)

  implicit class sopLogic(sop: SOP) {
    def modifySOP(children: List[SOP]): SOP = {
      sop match {
        case s: OperationNode => s.copy(sop = children)
        case s: Parallel => s.copy(sop = children)
        case s: Other => s.copy(sop = children)
        case s: Alternative => s.copy(sop = children)
        case s: Arbitrary => s.copy(sop = children)
        case s: Sequence => s.copy(sop = children)
        case s: SometimeSequence => s.copy(sop = children)
        case EmptySOP => EmptySOP
      }
    }

    def isEmpty = {sop == EmptySOP || (sop.sop.isEmpty && !sop.isInstanceOf[OperationNode])}

    def addChildren(children: Seq[SOP]): SOP = sop.modifySOP(sop.sop ++ children)

  }



    /**
      * this method extract all guards that the sop specify
      * @param group What group the generated condition belong to. Usually the SOPSpec name
      * @param getAllConditions Set true if "redundant" conditions should be extracted. (o1->o2->o3 will set o1f && o2f on o3)
      * @return a map between a operation id and a condition that the SOP specify.
      */
    def extractOperationCondition(sop: SOP, group: String, getAllConditions: Boolean = false): Map[ID, Condition] = {
      val props = findOpProps(sop, Map(), getAllConditions)
      props map{ case (id, props) =>
        val propList = if (props.size == 1) props.head else AND(props.toList)
        id -> Condition(propList, List(), SPAttributes("group" -> group, "kind" -> kind) )
      }
    }




    def extractOperationConditions(sops: List[SOP], group: String, getAllConditions: Boolean = false): Map[ID, Condition] = {
      extractOperationCondition(Parallel(sops),group, getAllConditions)
    }

    /**
      * If you end up with a List[Map[ID, Condition]], then this will merge them
      * @param maps
      * @return
      */
    def mergeConditionMaps(maps: List[Map[ID, Condition]]): Map[ID, List[Condition]] = {
      maps.foldLeft(Map[ID, List[Condition]]())((aggr, condMap) => {
        val update = condMap.map{case (id, cond) =>
          val conds = aggr.getOrElse(id, List[Condition]())
          id -> (cond :: conds)
        }
        aggr ++ update
      })
    }


    def getStartOperations(sop: SOP): Set[ID] = {
      getOps(sop,sops => Seq(sops.head))
    }

    def getFinalOperations(sop: SOP): Set[ID] = {
      getOps(sop, sops => Seq(sops.last))
    }

    def getAllOperations(sop: SOP): Set[ID] = {
      getOps(sop, sops => sops)
    }


    private def getOps(sop: SOP, seqEval: Seq[SOP] => Seq[SOP]) : Set[ID] = {
    sop match{
      case x: OperationNode => Set(x.operation)
      case x: SOP if x.isEmpty => Set()
      case x: Sequence => seqEval(x.sop).flatMap(s => getOps(s, seqEval)) toSet
      case x: SometimeSequence => seqEval(x.sop).flatMap(s => getOps(s, seqEval)) toSet
      case x: SOP => x.sop.flatMap(s => getOps(s, seqEval)) toSet
    }
  }


    def getCompleteProposition(sop: SOP): Proposition = {
      sop match {
        case x: OperationNode => EQ(x.operation, finished)
        case x: SOP if x.isEmpty => AlwaysTrue
        case x: Sequence => getCompleteProposition(x.sop.last)
        case x: SometimeSequence => getCompleteProposition(x.sop.last)
        case x: Alternative => OR(x.sop map getCompleteProposition toList)
        case x: SOP => AND(x.sop map getCompleteProposition toList)
      }
    }

    def getStartProposition(sop: SOP): Proposition = {
      sop match {
        case x: OperationNode => EQ(x.operation, init)
        case x: SOP if x.isEmpty => AlwaysTrue
        case x: Sequence => getStartProposition(x.sop.head)
        case x: SometimeSequence => getStartProposition(x.sop.head)
        case x: SOP => AND(x.sop map getStartProposition toList)
      }
    }

    def updateMap(newMap: Map[ID, Set[Proposition]], oldMap: Map[ID, Set[Proposition]]) = {
      val updates = newMap map{case (id, set) =>
        id -> (oldMap.getOrElse(id, Set()) ++ set)
      }
      oldMap ++ updates
    }

    def addPropToStartOps(sop: SOP, prop: Proposition): Map[ID, Set[Proposition]] = {
      getStartOperations(sop) map (_ -> Set(prop)) toMap
    }
    def addPropToOps(sop: SOP, prop: Proposition, toAllOps: Boolean): Map[ID, Set[Proposition]] = {
      val ops = if (toAllOps) getAllOperations(sop) else getStartOperations(sop)
      ops map (_ -> Set(prop)) toMap
    }

    def findOpProps(sop: SOP, map: Map[ID, Set[Proposition]], addToAll: Boolean = false): Map[ID, Set[Proposition]] = {
      sop match {
        case x: SOP if x.isEmpty => map
        case x: OperationNode => map // impl Hierarchy here later
        case x: SOP => {
          val childProps = x.sop.foldLeft(map) { (map, child) =>
            val props = findOpProps(child, map)
            updateMap(props, map)
          }
          x match {
            case alt: Alternative => {
              val startProps = alt.sop.map(c => c -> getStartProposition(c))
              val propsToAdd = startProps map { case (sop, prop) =>
                val otherProps = startProps.filter((kv) => kv._1 != sop) map (_._2)
                if (otherProps.size == 1) sop -> otherProps.head
                else sop -> AND(otherProps toList)
              }
              val newProps = propsToAdd.map { case (sop, prop) => addPropToOps(sop, prop, addToAll)}
              newProps.foldLeft(childProps) { case (oldMap, newMap) => updateMap(newMap, oldMap)}
            }
            case arbi: Arbitrary => {
              val startProps = arbi.sop.map(c => c -> getStartProposition(c))
              val complProps = arbi.sop.map(c => c -> getCompleteProposition(c)) toMap


              val props = startProps.map { case (sop, prop) =>
                sop -> OR(List(prop, complProps(sop)))
              }
              val propsToAdd = props map { case (sop, prop) =>
                val otherProps = props.filter((kv) => kv._1 != sop) map (_._2)
                if (otherProps.size == 1) sop -> otherProps.head
                else sop -> AND(otherProps toList)
              }
              val newProps = propsToAdd.map { case (sop, prop) => addPropToOps(sop, prop, addToAll)}
              newProps.foldLeft(childProps) { case (oldMap, newMap) => updateMap(newMap, oldMap)}

            }
            //TODO: Add sometime in sequence as well, but maybe we should not allow that specification? 140906
            case seq: Sequence => {
              def req(prevProp: Proposition, sops: List[SOP], res: Map[ID, Set[Proposition]]): Map[ID, Set[Proposition]] = {
                sops match {
                  case Nil => res
                  case x :: xs => {
                    val prop = getCompleteProposition(x)
                    if (prevProp == AlwaysTrue) req(prop, xs, res)
                    else {
                      val update = addPropToOps(x, prevProp, addToAll)
                      val accumulatedProp = if (addToAll) AND(List(prevProp, prop)) else prop
                      req(accumulatedProp, xs, updateMap(update, res))
                    }
                  }
                }
              }
              val res = req(AlwaysTrue, seq.sop.toList, Map())
              updateMap(res, childProps)
            }


            case _ => childProps
          }
        }
      }
    }


    /**
      * This method takes a sop and extract all relations defined by that SOP
      * @param sops
      */
    def extractRelations(sops: List[SOP]): Map[Set[ID], SOP] = {
      val result: Map[Set[ID], SOP] = sops match {
        case Nil => Map()
        case EmptySOP :: Nil => Map()
        case x :: xs => {
          val reqChildren = x.sop map extractOps toList
          val relMap = foldThem(x, reqChildren)
          val chMap = extractRelations(x.sop.toList)
          val rest = extractRelations(xs)
          relMap ++ chMap ++ rest
        }
      }
      result.filter(kv => !kv._2.isInstanceOf[Parallel])
    }

    def extractOps(sop: SOP): List[ID] = {
      def extr(xs: Seq[SOP]): List[ID] = xs flatMap extractOps toList

      sop match {
        case x: OperationNode => x.operation :: extr(x.sop)
        case x: SOP => extr(x.sop)
      }
    }

    def foldThem(parent: SOP, children: List[List[ID]]):Map[Set[ID], SOP] = {
      children match {
        case Nil => Map()
        case x :: Nil => Map()
        case x :: xs => {
          val map = for {
            head <- x
            other <- xs.flatten
          } yield Set(head, other)-> parent.modifySOP(List(head, other))
          map.toMap ++ foldThem(parent, xs)
        }
      }
    }




    def addMissingRelations(sops: List[SOP], relations: Map[Set[ID], SOP]): List[SOP] = {
      val sopRels = extractRelations(sops)
      val ops = sops flatMap getAllOperations
      val missing = (for {
        o1 <- ops
        o2 <- ops if o1 != o2 && !sopRels.contains(Set(o1, o2))
        rel <- relations.get(Set(o1, o2))
      } yield Set(o1, o2) -> rel).toMap
      val cond = makeProps(missing, relations)

      val otherOps = relations.keys flatMap(_ map(id => id)) filter(id => !ops.contains(id)) toSet
      val missingOthers = (for {
        o1 <- ops
        o2 <- otherOps
        rel <- relations.get(Set(o1, o2))
      } yield Set(o1, o2) -> rel).toMap
      val otherCond = makeProps(missingOthers, relations)

      val conds = makeConds(cond, otherCond)

      //println(s"conds: $cond")

      sops map(updateSOP(_, conds))
    }

    def makeProps(missing: Map[Set[ID], SOP], relations: Map[Set[ID], SOP]): Map[ID, Proposition] = {
      val res = missing.toList flatMap {
        case (_, s: Parallel) => List()
        case (_, s: Other) => List()
        case (_, s: Alternative) => {
          val temp = relOrder(s).map{case (id1, id2) => List(id1 -> EQ(id2, init), id2 -> EQ(id1, init))}
          temp.getOrElse(List())
        }
        case (_, s: Arbitrary) => {
          val temp = relOrder(s).map{case (id1, id2) => List(id1 -> NEQ(id2, executing), id2 -> NEQ(id1, executing))}
          temp.getOrElse(List())
        }
        case (_, s: Sequence) => {
          val temp = relOrder(s).map{case (id1, id2) => List(id2 -> EQ(id1, finished))}
          temp.getOrElse(List())
        }
        case (_, s: SometimeSequence) => {
          val temp = relOrder(s).map{case (id1, id2) => List(id2 -> OR(List(EQ(id1, init), EQ(id1, finished))))}
          temp.getOrElse(List())
        }
        case (_, _) => List()
      }

      val temp = res.foldLeft(Map[ID, AND]()){case (aggr, (id, prop)) =>
        if (!aggr.contains(id)) aggr + (id -> AND(List(prop)))
        else aggr + (id -> AND(prop :: aggr(id).props))
      }

      val filteredMap = temp.map{case (id, and) =>
        val seqs = and.props.flatMap{
          case EQ(SVIDEval(op), ValueHolder(st)) if op == id && st.asOpt[String].contains(finished) => Some(id)
          case _ => None
        }

        val removeIds = for {
          id1 <- seqs
          id2 <- seqs
          rels <- relations.get(Set(id1, id2)) if rels == Sequence(List(id1, id2))
        } yield {
          //println(s"remove $id1")
          id1
        }


        val filteredProps = and.props.filterNot{
          case EQ(SVIDEval(op), _) if op == id => removeIds.contains(id)
          case _ => false
        }
        id -> AND(filteredProps)
      }

      //println(s"temp: $temp" )
      //println(s"Filtered: $filteredMap" )

      filteredMap

    }


    def makeConds(c1: Map[ID, Proposition], c2: Map[ID, Proposition]): Map[ID, List[Condition]] = {
      val inC1AndBoth = c1 map{ case (id, prop) =>
        val cond1 = Condition(prop, List(), SPAttributes(
          "kind" -> kind,
          "group" -> "sop"
        ))
        id -> {
          if (c2.contains(id)){
            List(cond1, Condition(c2(id), List(), SPAttributes(
              "kind" -> kind,
              "group" -> "other"
            )))
          } else List(cond1)
        }
      }
      val onlyInC2 = c2.keySet.diff(c1.keySet)
      onlyInC2.foldLeft(inC1AndBoth){case (acc, id) =>
        acc + (id -> List(Condition(c2(id), List(), SPAttributes(
          "kind" -> kind,
          "group" -> "other"
        ))))
      }
    }

    def updateSOP(sop: SOP, conds: Map[ID, List[Condition]]): SOP = {
      val updCh = sop.sop.map(updateSOP(_, conds))
      val updSOP = if (updCh == sop.sop) sop else sop.modifySOP(updCh)
      updSOP match {
        case h: OperationNode => {
          if (conds.contains(h.operation)){
            OperationNode(h.operation, conds(h.operation), updCh)
          } else updSOP
        }
        case _ => updSOP
      }
    }

    def relOrder(sop: SOP): Option[(ID, ID)] = {
      sop.sop.toList match {
        case (h1: OperationNode) :: (h2: OperationNode) :: Nil => Some((h1.operation, h2.operation))
        case _ => None
      }
    }

  }


trait ActorStuff {
  import akka.actor._
  import akka.cluster.pubsub._
  import DistributedPubSubMediator.{ Put, Send, Subscribe, Publish }
  import scala.util.Try

  import play.api.libs.json._

  val system: ActorSystem
  val mediator = DistributedPubSub(system).mediator

  def fromSPMessageViaMediator(topic: String): Source[Map[String, SPValue], ActorRef] =
    Source
      .actorRef[String](1000, OverflowStrategy.dropHead)
      .mapMaterializedValue { ref => mediator ! DistributedPubSubMediator.Subscribe(topic, ref); ref }
      .map{json =>
        (for {
          message <- SPMessage.fromJson(json)
          header <- message.getHeaderAs[SPHeader]
          body <- message.getBodyAs[Map[String, SPValue]]
        } yield {
          body
        }).getOrElse(Map())
      }
}

trait CondStuff {
  case class cond(kind: String, guard: String, actions: String*)
  def c(kind: String, guard: String, actions: String*) = cond(kind, guard, actions:_*)
  def parse(c: cond)(idables: List[IDAble]) = {
    val g = if (c.guard.toLowerCase == "true") Some(AlwaysTrue)
    else if (c.guard.toLowerCase == "false") Some(AlwaysFalse)
    else PropositionParser(idables).parseStr(c.guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $c.guard: $err"); None
    }
    val xs = c.actions.map { action =>
      (ActionParser(idables).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }).get
    }
    Condition(g.get, xs.toList, attributes = SPAttributes("kind"->c.kind))
  }
}

trait ThingStuff {
  var things: List[Thing] = List.empty


  def v(name: String, initialState: Boolean): ID = {
    vm(name, SPValue(initialState), List(SPValue(false), SPValue(true)), Set())
  }

  def vm(name: String, initialState: Boolean, marked: Set[Boolean]): ID = {
    vm(name, SPValue(initialState), List(SPValue(false), SPValue(true)), marked.map(SPValue(_)))
  }

  def v(name: String, initialState: SPValue, domain: List[SPValue]): ID = {
    vm(name, initialState, domain, Set())
  }

  def vm(name: String, initialState: SPValue, domain: List[SPValue], marked: Set[SPValue], attr: SPAttributes = SPAttributes()): ID = {
    val t = Thing(name, SPAttributes("initialState" -> initialState, "domain" -> domain, "marked" -> marked).merge(attr))
    things = t :: things
    t.id
  }

  def i(name: String, initialState: Boolean): ID = {
    vm(name, SPValue(initialState), List(SPValue(false), SPValue(true)), Set(), SPAttributes("input" -> true))
  }

  def i(name: String, initialState: SPValue, domain: List[SPValue], activeWhen: Proposition = AlwaysTrue): ID = {
    vm(name, initialState, domain, Set(), SPAttributes("input" -> true, "activeWhen" -> activeWhen))
  }

  def o(name: String, initialState: Boolean): ID = {
    vm(name, SPValue(initialState), List(SPValue(false), SPValue(true)), Set(), SPAttributes("output" -> true))
  }

  def o(name: String, initialState: SPValue, domain: List[SPValue]): ID = {
    vm(name, initialState, domain, Set(), SPAttributes("output" -> true))
  }
}

trait Resource extends CondStuff with ThingStuff with ActorStuff {
  val id = ID.newID
  var abilities: List[Operation] = List.empty

  def makeResource(): SPResource

  def a(name: String, params: List[ID] = List())(conds: cond*): ID = {
    val conditions = conds.toList.map(c=>parse(c)(things))
    val attributes = SPAttributes("isa" -> "ability", "parameters" -> params)
    val ab = Operation(name, conditions, attributes = attributes)
    abilities = ab :: abilities
    ab.id
  }

  // mappers for drivers

  def stringToIDMapper(mapping: Map[String, ID]) =
    Flow[Map[String, SPValue]].map{ state =>
      mapping.flatMap { case (fieldname, id) => state.get(fieldname).map(spval => id -> spval) }
    }

  def IDToStringMapper(mapping: Map[ID, String]) =
    Flow[Map[ID, SPValue]].map{ state =>
      mapping.flatMap { case (id, fieldname) => state.get(id).map(spval => fieldname -> spval) }
    }

  def mapDomain(mapping: Map[ID, (SPValue => SPValue)]) =
    Flow[State].map{ state =>
      state ++ mapping.flatMap { case (id, f) => state.get(id).map(spval => id -> f(spval)) }
    }

}

trait MiniModel extends CondStuff with ThingStuff with ActorStuff with SynthesizeMiniModel with ExportNuXmvFile2 {
  var resources: Map[String, Resource] = Map.empty
  var operations: List[Operation] = List.empty
  var specs: List[SPSpec] = List.empty
  def use(name: String, resource: Resource) = resources = resources + (name -> resource)

  case class op(id: ID, name: String, ability : Option[String], bookings: Set[String], conds: List[cond], attr: SPAttributes)
  var ops: Map[String, op] = Map.empty

  def x(name: String, exprs: List[String]) = {
    val parseHelpers = resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList ++ operations
    val props = exprs.flatMap{x=>
      PropositionParser(parseHelpers ++ things).parseStr(x) match {
        case Right(p) => Some(p)
        case Left(err) => println(s"Parsing failed on forbidden expression: $x: $err"); None
      }
    }
    val spec = SPSpec(name, SPAttributes("forbiddenExpressions" -> props))
    specs = spec :: specs
    spec.id
  }


  // def o(name: String, ab: String, bookResource: String)(conds: cond*): ID =
  //   o(name, ab, Set(bookResource))(conds:_*)

  def updateGuard(guard: Proposition, remap: Map[ID, StateUpdater]): Proposition = {
    def updateID(pe: StateEvaluator): StateEvaluator = pe match {
      case SVIDEval(id) =>
        remap.get(id).map{
          case ASSIGN(oid) => SVIDEval(oid)
          case ValueHolder(spval) => ValueHolder(spval)
        }.getOrElse(SVIDEval(id))
      case x => x
    }

    guard match {
      case AND(xs) => AND(xs.map(x=>updateGuard(x, remap)))
      case OR(xs) => OR(xs.map(x=>updateGuard(x, remap)))
      case NOT(x) => NOT(updateGuard(x, remap))
      case EQ(left, right) => EQ(updateID(left), updateID(right))
      case NEQ(left, right) => NEQ(updateID(left), updateID(right))
      case GREQ(left, right) => GREQ(updateID(left), updateID(right))
      case LEEQ(left, right) => LEEQ(updateID(left), updateID(right))
      case GR(left, right) => GR(updateID(left), updateID(right))
      case LE(left, right) => LE(updateID(left), updateID(right))
      case AlwaysTrue => AlwaysTrue
      case AlwaysFalse => AlwaysFalse
    }
  }

  def filterProp(guard: Proposition, skip: List[ID]): Proposition = {
    guard match {
      case AND(xs) => AND(xs.map(x=>filterProp(x, skip)))
      case OR(xs) => OR(xs.map(x=>filterProp(x, skip)))
      case NOT(x) =>
        // not true, should still be true.... TODO: very dangerous fix this
        val res = filterProp(x, skip)
        if(res == AlwaysTrue) AlwaysTrue
        else NOT(res)
      case EQ(SVIDEval(id), SVIDEval(id2)) if skip.contains(id) && skip.contains(id2) => throw new Exception("assignment does not make sense: " + guard); AlwaysTrue
      case EQ(SVIDEval(id), SVIDEval(id2)) if skip.contains(id) || skip.contains(id2) => AlwaysTrue

      case EQ(SVIDEval(id), x) if skip.contains(id) => AlwaysTrue
      case EQ(x, SVIDEval(id)) if skip.contains(id) => AlwaysTrue

      case NEQ(SVIDEval(id), x) if skip.contains(id) => AlwaysTrue
      case NEQ(x, SVIDEval(id)) if skip.contains(id) => AlwaysTrue

      case GREQ(SVIDEval(id), x) if skip.contains(id) => AlwaysTrue
      case GREQ(x, SVIDEval(id)) if skip.contains(id) => AlwaysTrue

      case LEEQ(SVIDEval(id), x) if skip.contains(id) => AlwaysTrue
      case LEEQ(x, SVIDEval(id)) if skip.contains(id) => AlwaysTrue

      case GR(SVIDEval(id), x) if skip.contains(id) => AlwaysTrue
      case GR(x, SVIDEval(id)) if skip.contains(id) => AlwaysTrue

      case LE(SVIDEval(id), x) if skip.contains(id) => AlwaysTrue
      case LE(x, SVIDEval(id)) if skip.contains(id) => AlwaysTrue

      case x => x
    }
  }

  // todo: finish and move to domain
  def cleanProp(p: Proposition): Proposition = {
    p match {
      case AND(List()) => AlwaysTrue
      case AND(x::Nil) => x

      case AND(xs) if xs.exists(_==AlwaysFalse) => AlwaysFalse
      case AND(xs) =>
        val n = AND(xs.map(cleanProp).filterNot(_==AlwaysTrue))
        n match {
          case AND(List()) => AlwaysTrue
          case AND(x::Nil) => x
          case x => x
        }

      case OR(List()) => AlwaysTrue
      case OR(x::Nil) => x

      case OR(xs) if xs.exists(_==AlwaysTrue) => AlwaysTrue
      case OR(xs) =>
        val n = OR(xs.map(cleanProp))
        n match {
          case OR(List()) => AlwaysTrue
          case OR(x::Nil) => x
          case x => x
        }

      case NOT(x) if x == AlwaysFalse => AlwaysTrue
      case NOT(x) if x == AlwaysTrue => AlwaysFalse

      case NOT(x) => NOT(cleanProp(x))

      case x => x
    }
  }

  // def updateAction(action: Action, remap: Map[ID, StateUpdater]): Action = {
  //   val nid = remap.get(action.id).getOrElse(action.id)
  //   val nval = action.value match {
  //     case ASSIGN(id) => ASSIGN(remap.get(id).getOrElse(id))
  //     case x => x
  //   }
  //   action.copy(id = nid, value = nval)
  // }

  def getConds(conds: List[Condition], kind: String)  =
      conds.filter(_.attributes.getAs[String]("kind").getOrElse("")==kind)

  def makeOps() = {
    val allAbs = resources.map { case (rn, r) => r.abilities.map(a=>a.copy(name = rn + "." + a.name)) }.flatten.toList
    val allOps = ops.values.map(o => Operation(name = o.name, id = o.id)).toList
    val parseHelpers = allOps ++ resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList

    val abOps = ops.values.filter(_.ability.nonEmpty).map { o =>
      val ability = allAbs.find(_.name == o.ability.get).get
      val conditions = o.conds.map(c=>parse(c)(parseHelpers ++ things))

      import sp.runners.AbilityRunnerTransitions._
      val domain = List(AbilityStates.notEnabled, AbilityStates.enabled, AbilityStates.starting, AbilityStates.executing, AbilityStates.finished)

      // merge op with its ability => merging the conditions
      // but first, replace all occurences of the abiilty paremeters with assignment in the pre action
      val params = ability.attributes.getAs[List[ID]]("parameters").getOrElse(List())
      val update = params.flatMap{id =>
        val actions = getConds(conditions, "pre").map(_.action).flatten
        val assigns = actions.collect { case Action(`id`, x) => x }.headOption
        assigns.map(su => id -> su)
      }.toMap
      val updatedAbConds = ability.conditions.map{c =>
        val ng = updateGuard(c.guard, update)
        c.copy(guard = ng)
      }

      // amend isExecuting and isFinished conditions with the assignments made in the pre action
      // these should be invariant during operation execution
      val newConds = conditions ++ updatedAbConds
      val preActions = getConds(newConds, "pre").map(_.action).flatten
      val newGuards = preActions.map { a =>
        val assignTo = a.id
        val assignWhat = a.value match {
          case ASSIGN(id) => SVIDEval(id)
          case ValueHolder(spval) => ValueHolder(spval)
          case x => throw new Exception("AAAAAA"); ValueHolder(SPValue(x.toString)) // error
        }
        EQ(SVIDEval(assignTo), assignWhat)
      }

      // also amend them with the preconditions set out -- they are invariant unless they are inputs
      val skip = parseHelpers.filter(t => t.attributes.getAs[Boolean]("input").getOrElse(false)).map(_.id)
      // ... or assigned in the pre action
      val preAssigns = preActions.map(_.id)

      val extraIsExecConds = getConds(newConds, "pre").map(_.guard).map(g => filterProp(g, skip ++ preAssigns))
      //val extraIsExec = Condition(AND(newGuards ++ extraIsExecConds), List(), attributes = SPAttributes("kind"->"isExecuting"))
      // TODO: do we really need the above. I think not
      val extraIsExec = Condition(AND(newGuards), List(), attributes = SPAttributes("kind"->"isExecuting"))

      val extraPostConds = getConds(newConds, "pre").map(_.guard).map(g => filterProp(g, skip ++ preAssigns))
      //val extraPost = Condition(AND(newGuards ++ extraPostConds), List(), attributes = SPAttributes("kind"->"isFinished"))
      val extraPost = Condition(AND(newGuards), List(), attributes = SPAttributes("kind"->"isFinished"))

      val newConds2 = conditions ++ updatedAbConds ++ List(extraIsExec, extraPost)
      val newCondsCleaned = newConds2.map(c=>c.copy(guard = cleanProp(c.guard)))
      val op = Operation(o.name, newCondsCleaned, o.attr ++ SPAttributes("domain" -> domain, "ability" -> o.ability.get, "bookings" -> o.bookings), o.id)
      op
    }

    // op ops
    val opops = ops.values.filter(o=>o.ability.isEmpty && o.attr.getAs[String]("ability") == Some("yes")).map { o =>
      val conditions = o.conds.map(c=>parse(c)(parseHelpers ++ things))
      Operation(o.name, conditions, o.attr ++ SPAttributes("bookings" -> o.bookings), o.id)
    }

    // planning ops
    val plops = ops.values.filter(o=>o.ability.isEmpty && o.attr.getAs[Boolean]("hasGoal").getOrElse(false)).map { o =>
      val conditions = o.conds.map(c=>parse(c)(parseHelpers ++ things))
      Operation(o.name, conditions, o.attr, o.id)
    }

    operations = abOps.toList ++ opops.toList ++ plops.toList
  }

  // helper
  def o(name: String, ability: String, booking: String)(conds: cond*): ID = {
    o(name, Some(ability), Set(booking))(conds:_*)
  }

  def o(name: String, ability: Option[String] = None, bookings: Set[String] = Set(), attr: SPAttributes = SPAttributes())(conds: cond*): ID = {
    val updOp = ops.get(name) match {
      case Some(o) =>
        val updAb = if(o.ability.isEmpty && ability.nonEmpty) ability else o.ability
        val updBookings = o.bookings ++ bookings
        val updAttr = o.attr ++ attr
        val updConds = o.conds ++ conds.toList
        op(o.id, o.name, updAb, updBookings, updConds, updAttr)

      case None => op(ID.newID, name, ability, bookings, conds.toList, attr)
    }

    ops = ops + (name -> updOp)
    updOp.id
  }


  var sops: List[SOPSpec] = List.empty
  def sop(name: String, sop: List[SOP], attributes: SPAttributes = SPAttributes()) = {
    val s = SOPSpec(name, sop, attributes)
    sops = s :: sops
    s.id
  }

  // sop for control
  def sopC(name: String, sop: List[SOP], attributes: SPAttributes = SPAttributes()) = {
    val conds = SOPLogic2.extractOperationConditions(sop, s"fromSOP$name", true)
    val updOps = for {
      o <- operations
      c <- conds.get(o.id)
    } yield {
      o.copy(conditions = (c :: o.conditions ))
    }
    operations = operations.filterNot(o => updOps.exists(_.id == o.id)) ++ updOps

    val s = SOPSpec(name, sop, attributes)
    sops = s :: sops
    s.id
  }

  def exportNuXmv(filename : String = "dummy.smv"): Unit = {
    exportNuXmv(getIDAbles(), s"gitignore/$filename", Map(), AND(List()), "")
  }

  def synthesize(name: String = "dummy", runNow: Boolean = false) = {
    import scala.util.{Failure, Success, Try}
    Try[Unit] {
      val (updOps,_,_) = synthesizeModel(getIDAbles(), name, runNow)

      operations = operations.filterNot(o => updOps.exists(_.id == o.id)) ++ updOps
    } match {
      case Success(ops) =>
        println("Synthesis successful")
        ops
      case Failure(t) =>
        println("Synthesis failed: " + t.getMessage)
        t.printStackTrace
    }
  }

  def addBookings() = {
    val bookingMap = operations.foldLeft(Map[String, Set[ID]]()){ case (aggr, o) =>
      val bookingsForOp = o.attributes.getAs[Set[String]]("bookings").getOrElse(List())
      bookingsForOp.foldLeft(aggr){ case (aggr, resource) =>
        aggr + aggr.get(resource).map(s => resource -> (s + o.id)).getOrElse(resource -> Set(o.id))
      }
    }

    val starting = sp.runners.AbilityRunnerTransitions.AbilityStates.starting
    val executing = sp.runners.AbilityRunnerTransitions.AbilityStates.executing

    val newGuards = (for {
      (resource, ops) <- bookingMap
      o <- ops
    } yield {
      val others = ops - o
      val mutexes = others.toList.map{id =>
        val notStarting = NEQ(SVIDEval(id), ValueHolder(starting))
        val notExecuting = NEQ(SVIDEval(id), ValueHolder(executing))
        AND(List(notStarting, notExecuting))
      }
      if(mutexes.isEmpty) None
      else {
        val otherOpNames = operations.filter(o => others.contains(o.id)).map(_.name).mkString(",")
        val oName = operations.find(_.id == o).get.name
        println(s"Because of booking $resource by operation $oName, forbidding starting for other operations $otherOpNames")
        val cond = Condition(AND(mutexes), List(), SPAttributes("kind" -> "pre", "group" -> "resource"))
        Some(o -> cond)
      }
    }).flatten


    operations = operations.foldLeft(operations){ case (operations, o) =>
      val newCs = newGuards.filter(_._1 == o.id).map(_._2)
      val newO = o.copy(conditions = o.conditions ++ newCs)
      newO :: operations.filterNot(_.id == o.id)
    }
  }

  def getIDAbles(): List[IDAble] = {
    // save away the abilties so we can use them for debugging
    val abilities = resources.map { case (rn, r) => r.abilities.map(a=>a.copy(name = rn + "." + a.name)) }.flatten.toList
    val abilityThing = Thing("abilities", SPAttributes("modelAbilities" -> abilities, "notInModel" -> true))

    abilityThing :: operations ++ things ++ sops ++ specs ++ resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList
  }

  def makeResources(): List[SPResource] = {
    resources.map { case (name, r) => r.makeResource() }.toList
  }

  def getInitialState(): State = {
    val varState = things.flatMap(t => t.attributes.get("initialState").map(v => t.id -> v)).toMap
    val opState = operations.map(o => o.id ->
      SPValue(AbilityRunnerTransitions.AbilityStates.notEnabled) // TOOD: fix
    ).toMap

    varState ++ opState
  }

}
