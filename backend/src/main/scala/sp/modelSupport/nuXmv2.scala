package sp.modelSupport

import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.{ActionParser, PropositionParser, SOPLogic}

import scala.util.{Failure, Success, Try}
import sp.supremica._

trait ExportNuXmvFile2 {

  def computePlan(model: List[IDAble], state: Map[ID, SPValue], bound: Int, goal: Proposition, ltl: String, filename: String = "/tmp/runner.smv"):
      (List[String], Int, String, String) = {
    println("Running solver...")
    val t0 = System.nanoTime()

    exportNuXmv(model, filename, state, goal, ltl)

    import sys.process._
    import java.io.{InputStream,OutputStream,ByteArrayOutputStream,PrintWriter}

    val stdout = new scala.concurrent.SyncVar[List[String]]()
    val stderr = new scala.concurrent.SyncVar[List[String]]()

    def output(in: InputStream) {
      val lines = scala.io.Source.fromInputStream(in).getLines.toList
      in.close()
      stdout.put(lines)
    }

    def input(out: OutputStream) {
      out.write("go_bmc\n".getBytes)
      out.write(s"check_ltlspec_bmc_inc -k $bound\n".getBytes)
      out.write("quit\n".getBytes)
      out.close()
    }
    def error(err: InputStream) {
      val lines = scala.io.Source.fromInputStream(err).getLines.toList
      err.close()
      stderr.put(lines)
    }
    val pio = new ProcessIO(input _, output _, error _)
    val result = Process(s"/home/martin/bin/nuxmv -int $filename").run(pio).exitValue()
    val t1 = System.nanoTime()
    println("Time to solve: " + (t1 - t0) / 1e9d + " seconds. Error code: " + result)

    val stderrLines = stderr.get.mkString("\n")

    if(stderrLines.isEmpty) {
      val stdoutLines = stdout.get
      val numberOfTransitions = stdoutLines.reverse.find(line => line.contains("  -> State: 1.")).
        map(line => line.stripPrefix("  -> State: 1.").stripSuffix(" <-")).getOrElse("0").toInt - 1 // should be minus one!
      val s = stdoutLines.mkString("\n").replaceAll("(?m)^\\*\\*\\*.*?\n", "")
      val plan = s.split("\n").toList.filter(_.contains("_start = TRUE")).map(_.trim)
      val ops = model.collect { case op: Operation => op }
      val opNames = ops.map(o => "o_"+o.name.replaceAll("\\.", "_") -> o.name).toMap
      val p = plan.flatMap( str => opNames.get(str.stripSuffix("_start = TRUE")))
      println(s"New plan is, after $numberOfTransitions steps: " + p.mkString(","))
      (p, numberOfTransitions, stdoutLines.mkString("\n"), stderrLines)
    } else {
      println("Failed to compute plan!")
      println(stderrLines)
      (List(), 0, "", stderrLines)
    }
  }

  def spValTonuXmv(s: SPValue): String = s match {
    case play.api.libs.json.JsNumber(n) => n.toString
    case play.api.libs.json.JsBoolean(b) => if(b) "TRUE" else "FALSE"
    case play.api.libs.json.JsString(s) => s
    case _ => "ERROR"
  }

  def SPactionValTonuXmvSyntax(a: Action, vars: List[Thing]) = {
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

  def SPpropTonuXmvSyntax(p: Proposition, vars: List[Thing], ops: List[Operation]): String = {
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
    p match {
      case AND(Nil) => "TRUE"
      case AND(ps) => ps.map(p=>SPpropTonuXmvSyntax(p,vars,ops)).mkString("(", ")&(", ")")
      case OR(Nil) => "TRUE"
      case OR(ps) => ps.map(p=>SPpropTonuXmvSyntax(p,vars,ops)).mkString("(", ")|(", ")")
      case NOT(q) => s"!(${SPpropTonuXmvSyntax(q,vars,ops)})"
      // special case for operation state
      case EQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("finished") => ops.find(_.id==op).get.name + "_finished"
      case EQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("executing") => ops.find(_.id==op).get.name + "_executing"
      case EQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("starting") => "TRUE" // modifiedOps.find(_.id==op).get.name + "_starting"
      case EQ(l, r) => leftRight(l, "=", r)

      case NEQ(SVIDEval(op), ValueHolder(v)) if ops.exists(_.id == op) && v == SPValue("finished") => ops.find(_.id==op).get.name + "_finished"
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
  }


  import sp.runners.AbilityRunnerTransitions._
  val idle = "idle" // we dont keep track of enabled/not enabled

  def exportNuXmv(ids_ : List[IDAble], filename : String, initialState: Map[ID, SPValue], goal: Proposition, specs: String): Unit = {
    val ids = ids_.filterNot(_.attributes.getAs[Boolean]("notInModel").getOrElse(false))

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

    def propTonuXmvSyntax(p: Proposition) = SPpropTonuXmvSyntax(p,vars,ops)
    def actionValTonuXmvSyntax(a: Action) = SPactionValTonuXmvSyntax(a,vars)

    val sopSpecs = ids.filter(_.isInstanceOf[SOPSpec]).map(_.asInstanceOf[SOPSpec])
    val spSpecs = ids.filter(_.isInstanceOf[SPSpec]).map(_.asInstanceOf[SPSpec])

    // use inputs to rewrite our conditions
    val inputs = vs.filter(_.attributes.getAs[Boolean]("input").getOrElse(false)).map(_.id)

    var lines = "-- " + filename + " - created at " + new java.util.Date().toString + " --\n"

    lines += "MODULE main\n"

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
      val fin = AND(getConds(o.conditions, "isFinished").map(_.guard))
      val reset = AND(getConds(o.conditions, "reset").map(_.guard))  // TODO

      if(fin != AND(List()))
        lines += "DEFINE " + o.name + "_finished := " + propTonuXmvSyntax(fin) + ";\n"

      if(exec != AND(List()))
        // lines += "DEFINE " + o.name + "_executing := !" + o.name + "_finished &" + propTonuXmvSyntax(exec) + ";\n"
        lines += "DEFINE " + o.name + "_executing := " + propTonuXmvSyntax(exec) + ";\n"

      if(pre != AND(List()))
        //lines += "DEFINE " + o.name + "_enabled := !" + o.name + "_executing &" + propTonuXmvSyntax(pre) + ";\n"
        lines += "DEFINE " + o.name + "_enabled := " + propTonuXmvSyntax(pre) + ";\n"
    }

    // add assignment of initial states
    lines += "\n\n"
    lines += "ASSIGN\n"
    vars.foreach { v =>
      val init = initialState.get(v.id) orElse v.attributes.getAs[SPValue]("initialState")
      val initStr = spValTonuXmv(init.getOrElse(SPValue("[initial state not set]")))
      lines += "  init(" + v.name + ") := " + initStr + ";\n"
    }

    lines += "\n\n";

    def getConds(conds: List[Condition], kind: String)  =
      conds.filter(_.attributes.getAs[String]("kind").getOrElse("")==kind)


    // for convenience, start variables can only change when we can actually start the op, i.e. when we are enabled
    opStartVars.foreach { v =>
      lines += s"  next(${v.name}) := case\n"
      val o = ops.find(o => v.name == ostart(o)).get
      val pre = AND(getConds(o.conditions, "pre").map(_.guard))
      if(pre != AND(List()))
        lines += s"    ${o.name}_enabled : {FALSE,TRUE};\n"
      lines += s"    TRUE : FALSE;\n"
      lines += "  esac;\n\n"
    }

    lines += "\n\n";

    // group operations based on each variable that appears in them
    // then for each variable, add transitions actions

    lines += "-- VARIABLES -- \n"
    lines += "\n"

    vs.foreach { v =>

      lines += s"  next(${v.name}) := case\n"

      if(inputs.contains(v.id)) {
        // for inputs (and only inputs!) we check if there are any effects associated with them
        ops.foreach { o =>
          val startEffects = getConds(o.conditions, "startEffect").map(_.action).flatten.filter(a=>a.id==v.id)
          val executingEffects = getConds(o.conditions, "executingEffect").map(_.action).flatten.filter(a=>a.id==v.id)

          if(startEffects.nonEmpty) {
            val startString = startEffects.map(actionValTonuXmvSyntax).mkString(",")
            lines += s"    !${ostart(o)} & next(${ostart(o)}) : {$startString} ;   --- sp start effect\n"
          }

          if(executingEffects.nonEmpty) {
            val executingString = executingEffects.map(actionValTonuXmvSyntax).mkString(",")
            lines += s"    ${o.name}_executing : {$executingString, ${v.name}} ;   --- sp executing effect\n"
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


          val postActions = getConds(o.conditions, "isFinished").map(_.action).flatten.filter(_.id == v.id)
          postActions.foreach { pa =>
            val actionStr = actionValTonuXmvSyntax(pa)

            // lines += s"    ${o.name}_executing & next(${o.name}_finished) : $actionStr;\n"
            lines += s"    ${o.name}_finished : $actionStr;\n"
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

    if(specs.nonEmpty) lines += s"LTLSPEC $specs ;"
    else {
      val goalStr= propTonuXmvSyntax(goal)
      lines += s"LTLSPEC ! F ( $goalStr );"
    }

    lines += "\n"
    lines += "\n"

    import java.io.PrintWriter
    new PrintWriter(filename) { write(lines); close() }
  }
}
