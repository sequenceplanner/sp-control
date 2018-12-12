package sp.modelSupport

import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.{ActionParser, PropositionParser, SOPLogic}

import scala.util.{Failure, Success, Try}
import sp.supremica._

trait ExportNuXmvFile2 {

  import sp.runners.AbilityRunnerTransitions._
  val idle = "idle" // we dont keep track of enabled/not enabled

  def exportNuXmv(ids: List[IDAble], filename : String, initialState: Map[ID, SPValue], specs: String): Unit = {

    // Extract from IDAbles

    // nuxmv cannot handle "." in identifiers. do some renaming
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation]).
      filterNot(_.attributes.getAs[String]("isa") == Some("Ability")).map(o => o.copy(name = "o_"+o.name.replaceAll("\\.", "_")))

    val vs = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing]).map(v => v.copy(name = "v_"+v.name.replaceAll("\\.", "_")))

    def ostart(o: Operation) = o.name+"_start"

    val opStartVars = ops.map{o =>
      Thing(ostart(o), SPAttributes("initialState" -> SPValue(false), "domain" -> List(false, true)))
    }

    val vars = vs ++ opStartVars

    val sopSpecs = ids.filter(_.isInstanceOf[SOPSpec]).map(_.asInstanceOf[SOPSpec])
    val spSpecs = ids.filter(_.isInstanceOf[SPSpec]).map(_.asInstanceOf[SPSpec])

    // use inputs to rewrite our conditions
    val inputs = vs.filter(_.attributes.getAs[Boolean]("input").getOrElse(false)).map(_.id)

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

    lines += "-- DEFINES for operation states --\n"

    // the state of each op is a projection of the local state
    ops.foreach { o =>
      val pre = AND(getConds(o.conditions, "pre").map(_.guard))
      val exec = AND(getConds(o.conditions, "isExecuting").map(_.guard))
      val fin = AND(getConds(o.conditions, "post").map(_.guard))
      val reset = AND(getConds(o.conditions, "reset").map(_.guard))

      lines += "DEFINE " + o.name + "_finished := " + propTonuXmvSyntax(fin) + ";\n"
      lines += "DEFINE " + o.name + "_executing := !" + o.name + "_finished &" + propTonuXmvSyntax(exec) + ";\n"
      lines += "DEFINE " + o.name + "_enabled := !" + o.name + "_executing &" + propTonuXmvSyntax(pre) + ";\n"
    }

    // add assignment of initial states
    lines += "\n\n"
    lines += "ASSIGN\n"
    vars.foreach { v =>
      val init = initialState.get(v.id) orElse v.attributes.getAs[SPValue]("initialState")
      val initStr = spValTonuXmv(init.getOrElse(SPValue("[initial state not set]")))
      lines += "  init(" + v.name + ") := " + initStr + ";\n"
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
      case AND(Nil) => "TRUE"
      case AND(ps) => ps.map(propTonuXmvSyntax).mkString("(", ")&(", ")")
      case OR(Nil) => "TRUE"
      case OR(ps) => ps.map(propTonuXmvSyntax).mkString("(", ")|(", ")")
      case NOT(q) => s"!${propTonuXmvSyntax(q)}"
        // special case for operation state
      case EQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("executing") => ops.find(_.id==op).get.name + "_executing"
      case EQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("starting") => "TRUE" // modifiedOps.find(_.id==op).get.name + "_starting"
      case EQ(l, r) => leftRight(l, "=", r)

      case NEQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("executing") => "!" + ops.find(_.id==op).get.name + "_executing"
      case NEQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("starting") => "TRUE"// "!" + modifiedOps.find(_.id==op).get.name + "_starting"
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


    // for convenience, start variables can only change when we can actually start the op, i.e. when we are enabled
    opStartVars.foreach { v =>
      lines += s"  next(${v.name}) := case\n"
      val o = ops.find(o => v.name == ostart(o)).get
      lines += s"    ${o.name}_enabled : {FALSE,TRUE};\n"
      lines += s"    TRUE : FALSE;\n"
      lines += "  esac;\n\n"
    }

    lines += "\n\n";

    // group operations based on each variable that appears in them
    // then for each variable, add transitions actions

    lines += "-- VARIABLES -- \n"
    lines += "\n"

    def findInputGuard(guard: Proposition, input: ID): List[Action] = {
      guard match {
        case AND(xs) => xs.map(x=>findInputGuard(x, input)).flatten
        case OR(xs) => xs.map(x=>findInputGuard(x, input)).flatten
        case NOT(x) => findInputGuard(x, input)
        case EQ(SVIDEval(id), SVIDEval(id2)) if inputs.contains(id) && inputs.contains(id2) => throw new Exception("assignment does not make sense: " + guard); List()

        case EQ(SVIDEval(id), SVIDEval(id2)) if id == input && !inputs.contains(id2) => List(Action(id, ASSIGN(id2)))
        case EQ(SVIDEval(id), SVIDEval(id2)) if !inputs.contains(id) && id2 == input => List(Action(id2, ASSIGN(id)))

        case EQ(SVIDEval(id), ValueHolder(x)) if id == input => List(Action(id, ValueHolder(x)))
        case EQ(ValueHolder(x), SVIDEval(id)) if id == input => List(Action(id, ValueHolder(x)))

        // todo, check for inequalities.... these cannot be handled
        case x => List()
      }
    }

    def findAssignToSPVal(o: Operation, assignee: ID):Option[SPValue] = {
      val allActions = getConds(o.conditions, "pre").map(_.action).flatten
      val x = allActions.collect { case Action(`assignee`, ValueHolder(spval)) => spval }
      x.headOption
    }

    vs.foreach { v =>

      lines += s"  next(${v.name}) := case\n"

      if(inputs.contains(v.id)) {
        // check post guards for inputs. special treatment for them!
        ops.foreach { o =>
          val postGuards = getConds(o.conditions, "post").map(_.guard)
          val inputGuards = postGuards.map(p=>findInputGuard(p, v.id)).flatten

          if(inputGuards.size > 1) {
            println(o.name)
            println(o.conditions)
            println(inputGuards)
          }
          assert(inputGuards.size < 2)
          inputGuards.headOption.foreach { ig =>
            // val nv = ig.value match {
            //   case ASSIGN(other) =>
            //     // check if input guard has a corresponding pre action that sets the expected value
            //     findAssignToSPVal(o, other).map(spval => ValueHolder(spval)).getOrElse(ASSIGN(other))
            //   case x => x
            // }
            val igStr = actionValTonuXmvSyntax(ig) // .copy(value = nv))
            lines += s"    ${o.name}_executing : {$igStr, ${v.name}} ;   --- post-guard from sp model transformed into this\n"
          }

        }
      } else {

        // find all variable assignments in the ops
        val assignsToMe = ops.foreach { o =>
          val preActions = getConds(o.conditions, "pre").map(_.action).flatten.filter(_.id == v.id)

          preActions.foreach { pa =>
            val actionStr = actionValTonuXmvSyntax(pa)

            lines += s"    !${ostart(o)} & next(${ostart(o)}) : $actionStr;\n"
          }


          val postActions = getConds(o.conditions, "post").map(_.action).flatten.filter(_.id == v.id)
          postActions.foreach { pa =>
            val actionStr = actionValTonuXmvSyntax(pa)

            lines += s"    ${o.name}_executing & next(${o.name}_finished) : $actionStr;\n"
          }
        }
      }

      lines += s"    TRUE : ${v.name};\n" // default case, no transition taken
      lines += "  esac;\n\n"

    }


    lines += "\n"
    lines += "\n"

    lines += "INVAR\n"

    lines += "  count(" + opStartVars.map(_.name).mkString(", ") + ") <= 1; -- only one op transition can be taken at a time \n"

    lines += "\n"
    lines += "\n"

    lines += specs

    lines += "\n"
    lines += "\n"

    import java.io.PrintWriter
    new PrintWriter(filename) { write(lines); close() }
  }
}
