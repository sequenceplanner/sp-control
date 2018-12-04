package sp.modelSupport

import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.{ActionParser, PropositionParser, SOPLogic}

import scala.util.{Failure, Success, Try}
import sp.supremica._

trait ExportNuXmvFile {

  import sp.runners.AbilityRunnerTransitions._
  val idle = "idle" // we dont keep track of enabled/not enabled

  def exportNuXmv(ids: List[IDAble], filename : String): Unit = {

    // Extract from IDAbles

    // supremica cannot handle "." in strings... work around
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation]).
      filterNot(_.attributes.getAs[String]("isa") == Some("Ability")).map(o => o.copy(name = "o_"+o.name.replaceAll("\\.", "_")))

    val vs = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing]).map(v => v.copy(name = "v_"+v.name.replaceAll("\\.", "_")))

    def ostart(o: Operation) = o.name+"_start"

    val opVars = ops.map{o =>
      val domain = List(idle, AbilityStates.starting, AbilityStates.executing, AbilityStates.finished).map(SPValue(_))
      Thing(o.name, SPAttributes("initialState" -> SPValue(idle), "domain" -> domain), id = o.id)
    }
    val opStartVars = ops.map{o =>
      Thing(ostart(o), SPAttributes("initialState" -> SPValue(false), "domain" -> List(false, true)))
    }

    val vars = vs ++ opVars ++ opStartVars

    val sopSpecs = ids.filter(_.isInstanceOf[SOPSpec]).map(_.asInstanceOf[SOPSpec])
    val spSpecs = ids.filter(_.isInstanceOf[SPSpec]).map(_.asInstanceOf[SPSpec])

    // use inputs to rewrite our conditions
    val inputs = vs.filter(_.attributes.getAs[Boolean]("input").getOrElse(false)).map(_.id)

    // hack because im lazy :(
    var tempActions: List[Action] = List()
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

    val modifiedOps = ops.map { o =>
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

    var lines = "-- " + filename + " - created at " + new java.util.Date().toString + " --\n"

    lines += "MODULE main\n"

    def spValTonuXmv(s: SPValue): String = s match {
      case play.api.libs.json.JsNumber(n) => n.toString
      case play.api.libs.json.JsBoolean(b) => if(b) "TRUE" else "FALSE"
      case play.api.libs.json.JsString(s) => s
      case _ => "ERROR"
    }

    // add variables and their domains
    lines += "VAR\n"
    vars.foreach { v => for {
      domain <- v.attributes.getAs[List[SPValue]]("domain")
    } yield {
      if(domain == List(SPValue(false), SPValue(true))) // hack
        lines += "  " + v.name + " : boolean;\n"
      else
        lines += "  " + v.name + " : {" + domain.map(_.toString.stripPrefix("\"").stripSuffix("\"")).mkString(", ") + "};\n"
    }
    }

    // add assignment of initial states
    lines += "\n\n"
    lines += "ASSIGN\n"
    (vars).foreach { v => for {
      is <- v.attributes.getAs[SPValue]("initialState")
    } yield {
      lines += "  init(" + v.name + ") := " + spValTonuXmv(is) + ";\n"
    }
    }

    def actionValTonuXmvSyntax(a: Action) = {
      a.value match {
        case ValueHolder(spval) => spValTonuXmv(spval)
        case ASSIGN(id) =>
          val name = vars.find(_.id==id).get.name // die if we are not careful
                                                  // dont look up in domain for names
          name
        case other =>
          println(s"actionTonuXmvSyntax cannot handle: $other right now. sorry")
          other.toString
      }
     }

    def propTonuXmvSyntax(p: Proposition): String = p match {
      case AND(ps) => ps.map(propTonuXmvSyntax).mkString("(", ")&(", ")")
      case OR(ps) => ps.map(propTonuXmvSyntax).mkString("(", ")|(", ")")
      case NOT(q) => s"!${propTonuXmvSyntax(q)}"
      case EQ(l, r) => leftRight(l, "=", r)
      case NEQ(l, r) => leftRight(l, "!=", r)
      case GREQ(l, r) => leftRight(l, ">=", r)
      case GR(l, r) => leftRight(l, ">", r)
      case LEEQ(l, r) => leftRight(l, "<=", r)
      case LE(l, r) => leftRight(l, "<", r)
      case AlwaysTrue => "TRUE"
      case AlwaysFalse => "FALSE"
      case other =>
        println(s"propTonuXmvSyntax cannot handle: $other right now. sorry")
        other.toString
    }

    def leftRight(l: StateEvaluator, operator: String, r: StateEvaluator) = {
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
          spValTonuXmv(spval)

      }

      s"$left $operator $right"
    }

    lines += "\n\n";

    def getConds(conds: List[Condition], kind: String)  =
      conds.filter(_.attributes.getAs[String]("kind").getOrElse("")==kind)


    // for convenience, start variables can only change when we can actually start the op...
    opStartVars.foreach { v =>
      lines += s"  next(${v.name}) := case\n"

      val o = modifiedOps.find(o => v.name == ostart(o)).get

      val isInit = EQ(o.id, ValueHolder(SPValue(idle)))
      val startRequest = opStartVars.find(_.name == ostart(o)).map(_.name).get
      val isStartingEnabled = s"next($startRequest)=TRUE" // EQ(startRequest, ValueHolder(SPValue(true)))
      val check = AND(List(isInit) ++ getConds(o.conditions, AbilityKinds.pre).map(_.guard))
      val checkStr = propTonuXmvSyntax(check)
      lines += s"    $checkStr : {FALSE,TRUE};\n"
      lines += s"    TRUE : FALSE;\n"
      lines += "  esac;\n\n"
    }

    lines += "\n\n";

    lines += "-- Operation transitions -- \n"
    // start with the transitions for the operation state variables
    modifiedOps.foreach { o =>

      lines += s"  next(${o.name}) := case\n"

      // init to starting transition
      {
        val isInit = EQ(o.id, ValueHolder(SPValue(idle)))
        val startRequest = opStartVars.find(_.name == ostart(o)).map(_.name).get
        val isStartingEnabled = s"next($startRequest)=TRUE" // EQ(startRequest, ValueHolder(SPValue(true)))
        val check = AND(List(isInit) ++ getConds(o.conditions, AbilityKinds.pre).map(_.guard))
        val checkStr = propTonuXmvSyntax(check)
        lines += s"    $isStartingEnabled & $checkStr : ${AbilityStates.starting};\n"
      }

      // starting to executing transition
      {
        val isStarted = EQ(o.id, ValueHolder(SPValue(AbilityStates.starting)))
        val check = AND(List(isStarted) ++ getConds(o.conditions, AbilityKinds.started).map(_.guard))
        val checkStr = propTonuXmvSyntax(check)
        lines += s"    $checkStr : ${AbilityStates.executing};\n"
      }

      // executing to finished transition
      {
        val isExec = EQ(o.id, ValueHolder(SPValue(AbilityStates.executing)))
        val check = AND(List(isExec) ++ getConds(o.conditions, AbilityKinds.post).map(_.guard))
        val checkStr = propTonuXmvSyntax(check)
        lines += s"    $checkStr : ${AbilityStates.finished};\n"
      }

      // finished to idle (reset) transition
      {
        val isFin = EQ(o.id, ValueHolder(SPValue(AbilityStates.finished)))
        val check = AND(List(isFin) ++ getConds(o.conditions, AbilityKinds.reset).map(_.guard))
        val checkStr = propTonuXmvSyntax(check)
        lines += s"    $checkStr : idle;\n"
      }

      lines += s"    TRUE : ${o.name};\n" // default case, no transition taken
      lines += "  esac;\n\n"
    }

    // group operations based on each variable that appears in them
    // then for each variable, add transitions actions

    lines += "\n"
    lines += "\n"

    vs.foreach { v =>

      lines += s"  next(${v.name}) := case\n"

      // find all variable assignments in the ops
      val assignsToMe = modifiedOps.foreach { o =>
        val preActions = getConds(o.conditions, "pre").map(_.action).flatten.filter(_.id == v.id)

        preActions.foreach { pa =>
          val actionStr = actionValTonuXmvSyntax(pa)

          lines += s"    ${o.name} = idle & next(${o.name}) = starting : $actionStr;\n"
        }

        val postActions = getConds(o.conditions, "post").map(_.action).flatten.filter(_.id == v.id)

        postActions.foreach { pa =>
          val actionStr = actionValTonuXmvSyntax(pa)

          lines += s"    ${o.name} = ${AbilityStates.executing} & next(${o.name}) = ${AbilityStates.finished} : $actionStr;\n"
        }
      }

      lines += s"    TRUE : ${v.name};\n" // default case, no transition taken
      lines += "  esac;\n\n"

    }


    lines += "\n"
    lines += "\n"

    lines += "INVAR\n"

    lines += "  count(" + opStartVars.map(_.name).mkString(", ") + ") <= 1; -- only one op transition can be taken at a time \n"

    import java.io.PrintWriter
    new PrintWriter(filename) { write(lines); close() }
  }
}
