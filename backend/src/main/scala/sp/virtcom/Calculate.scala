package sp.virtcom

import akka.stream.actor.ActorPublisherMessage.Request
import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.SOPLogic._
import sp.patrikmodel.{CollectorModel}
import oscar.cp._
import scala.concurrent.Future


object Calculate extends cal
trait cal {

  def synthOpt(SopID : ID, ids : List[IDAble], neglectedCases : Set[ID]) ={
    val checkedTime = true

    val activeStruct = ids.filter(_.isInstanceOf[Struct]).map(_.asInstanceOf[Struct]).find(struct => struct.items.map(sn => sn.item).contains(SopID)).get
    val idsInStruct = ids.filter(i => activeStruct.items.map(_.item).contains(i.id))

    var sopSpec = idsInStruct.find(idable => idable.id == SopID).get.asInstanceOf[SOPSpec]
    var plcSOP = idsInStruct.find(idable => idable.name == "PLC_SOP").get.asInstanceOf[SOPSpec]
    var zonespecs = idsInStruct.filter(_.isInstanceOf[SOPSpec]).map(_.asInstanceOf[SOPSpec]).diff(List(sopSpec)).diff(List(plcSOP)).toIterable

    val h = sopSpec.attributes//.getAs[sp.domain.SPAttributes]("hierarchy").getOrElse("")

    val ops = idsInStruct.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])

   //-------------------------------------------------------------------------------------

    case class VolvoRobotScheduleCollector(val modelName: String = "VolvoRobotSchedule") extends CollectorModel
    val collector = VolvoRobotScheduleCollector() // This can collect all of the operations and create transition variables between them
    val idle = "idle" // This is a value for the variables created in the collector, and it should later also be assigned to the first operation(s) in a schedule (SOP)

    // Go through the list of (operations) get their schedule names and create Collector variables.
    ops.map{op => op.attributes.getAs[String]("robotSchedule").getOrElse("error")}.distinct.foreach{s => collector.v(s, idleValue = Some(idle), attributes = h)}

    //-------------------------------------------------------------------------------------


    // Go through the SOPs for each resource, one by one and create straight operation sequences for the optimization.
    // Create new IDs for the operations, and new SOPs for those IDs
    var opSequences = List[List[Operation]]() // A list with lists for all possible sequences
    var ss = List[SOP]() // The SOP
    sopSpec.sop.foreach(individualSop => {
      var exOps =  extractFromSOP(individualSop :SOP, "idle", true, collector, ops, h, checkedTime)
      ss :+= exOps._3
      opSequences ++= exOps._1
    })

    sopSpec = SOPSpec(sopSpec.name + "_Synth", ss,h)

    var operations = collector.operations // Get all operations from the collector

    var uids = collector.parseToIDablesWithIDs

    plcSOP.sop.foreach(individualSop => {
      var exOps =  extractFromPLCSOP(individualSop :SOP, operations) // Changed to List[op] from set, Todo: check that the order of the operations do not mess anything up
      exOps.foreach(oplist => {oplist.foreach(op => println("op :" + op.name + ", ID :" + op.id)); println("\n")})
      opSequences ++= exOps
    })

    //-------------------------------------------------------------------------------------

    var zoneMapping = Map[Operation,Set[String]]() // Create op zone Map
    operations.foreach(op => zoneMapping += (op -> Set())) // fill the map with all operations

    var zonespecsNew = List[SOPSpec]() // Create a new zonespec with the new operation ids and create the zone mapping

    var zoneMappingList = List[Map[Operation,Set[String]]]()
    zonespecs.foreach(zSpec=> {
      val zoneName =zSpec.name
      var zonesops = List[SOP]()
      zSpec.sop.foreach(sopNode => {
        zonesops :+= zoneSpecsAndZoneMap(sopNode, zoneName)._1
      })
      zonespecsNew :+= SOPSpec(zoneName, zonesops, h)
    })
    zonespecs = zonespecsNew.toIterable


    def zoneSpecsAndZoneMap(sopNode :SOP, zoneName: String) : (SOP, Map[Operation,Set[String]])={
      var zoneMappingNew = Map[Operation,Set[String]]()
      var newSop = SOP()
      if(sopNode.isInstanceOf[Arbitrary]) {
        var arbSeqs = Seq[SOP]()
        sopNode.sop.foreach(subSopNode => {arbSeqs :+= zoneSpecsAndZoneMap(subSopNode, zoneName)._1; zoneMappingList :+= zoneSpecsAndZoneMap(subSopNode, zoneName)._2})
        newSop = Arbitrary(arbSeqs.toList)
      }
      else if (sopNode.isInstanceOf[Sequence]){
        var seqSeqs = Seq[SOP]()
        sopNode.sop.foreach(subSopNode => {seqSeqs :+= zoneSpecsAndZoneMap(subSopNode, zoneName)._1; zoneMappingNew ++= zoneSpecsAndZoneMap(subSopNode, zoneName)._2})
        newSop = Sequence(seqSeqs.toList)
      }
      else if (sopNode.isInstanceOf[OperationNode]){
        val op = operations.filter(op => op.attributes.getAs[String]("original").getOrElse("error") == sopNode.asInstanceOf[OperationNode].operation.toString).head
        zoneMapping += ( op -> (zoneMapping(op) + zoneName))
        zoneMappingNew += ( op -> Set(zoneName))

        newSop = SOP(OperationNode(op.id))
      }
      (newSop, zoneMappingNew)
    } // Create a zonemap and a new zonespec with the new operation ids.


    //-------------------------------------------------------------------------------------

    val zones = zoneMapping.map { case (o, zones) => zones }.flatten.toSet // All the zone names as a set of strings
    var mutexes: List[(ID,ID)] = List() // Should contain all a list for all operations in zones (that should not execute at the same time) ex: Zone 1: op1, op2, op3 -> mutexes: (op1,op2),(op1,op3),(op2,op3)

    // Creates the mutexes
    val zoneSeqs = zones.map { zone =>
      val opsInZone = zoneMapping.filter { case (o,zones) => zones.contains(zone) }.map(_._1)

      val forbiddenPairs = (for {
        o1 <- opsInZone
        o2 <- opsInZone if o1 != o2
      } yield {
        Set(o1,o2)
      }).toSet

      val l = (if(forbiddenPairs.nonEmpty) {

        val forbiddenOps = forbiddenPairs.map{ s =>
          val o1 = s.toList(0)
          val o2 = s.toList(1)
          mutexes:+=(o1.id,o2.id)
          Set(o1.id,o2.id)
        }.toList.flatten.distinct
        // collector.x(zone, operations=forbiddenOps, attributes=h)
        forbiddenOps
      } else List())
      zone -> l
    }.toMap

    mutexes = mutexes.distinct

    var precedences: List[(ID,ID)] = List()
    var forceEndTimes: List[(ID,ID)] = List()

    var approvedOps = List[Operation]()  // Create a list where all the wanted operations from the next step can be saved
    opSequences.foreach(oplist => {  // going through each oplist and adding information to the precedence and forceEndTime lists

      var opListIds = oplist.map(op => op.attributes.getAs[ID]("original").get)

      if (opListIds.intersect(neglectedCases.toList).isEmpty){ // Check that the current oplist does not have an operation which was not selected during the alternative selection


        operations = updateOpSequenceFromCollector(oplist , collector) // Get updated operation Ids and attributes from collector, not sure this is actually needed anymore...
        approvedOps ++= operations // add all of the operations of the oplist to a list of operations for the later optimization

        // Creates a new Zonemap where all of the zonemap operations are contained in the current oplist
        var zoneMappingNew = Map[Operation, Set[String]]()
        operations.foreach(op => zoneMappingNew += (op -> Set()))
        zoneMappingList.foreach(zMap => {
          if (zMap.keySet subsetOf operations.toSet) {
            zMap.keys.foreach(op => zoneMappingNew += (op -> (zoneMappingNew(op) ++ zMap(op))))
          }
        })

        var zoneMap = zoneMappingNew //zoneMapping

        // Creates a new ZoneMap for the current operation Sequence, containing only those operations.

        // makes sure that the zonemap has the right IDs. This might also be redundant now... If not this could probably be done earlier
        var newZoneMap = Map[Operation, Set[String]]()
        zoneMap.map { case (o, zoness) => {
          operations.foreach({ opWithTrans => if (opWithTrans.id == o.id) newZoneMap += (opWithTrans -> zoness) })
        }
        }
        zoneMap = newZoneMap


        // For the CP solver-----------
        if (operations.size > 1) {
          val np = operations zip operations.tail
          precedences ++= np.map { case (o1, o2) => (o1.id, o2.id) } // Orders all operations after eachother in the order they were given from the op list
          if (!zoneMap.isEmpty) {
            // Checks which operations have the same zoneMap
            val fe = np.filter { case (o1, o2) => zoneMap(o1) == zoneMap(o2) }.map { case (o1, o2) => (o1.id, o2.id) } // Seems to do it like: If there are more than one operation with the same zones, then they are placed in fe. This will speed up the optimization in some way.
            // Like: op1: in zones: Station1,Zone1
            //       op2: in zones: Station1,Zone1 --> fe: (op1,op2),
            forceEndTimes ++= fe
          }
        }
      }
    })


    approvedOps = approvedOps.distinct
    val approvedIds = approvedOps.map(op => op.id)

    mutexes = mutexes.distinct
    forceEndTimes = forceEndTimes.distinct
    precedences = precedences.distinct

    mutexes = mutexes.filter(m => (approvedIds.contains(m._1) && approvedIds.contains(m._2)))
    // When all of the operation sequences have been processed and the precedences + forceEndTimes have been created.
    // The mutexes which are just given by the great zonemap and all operations + precedences & forceEndTimes are sent to the robot optimization.

    val ro = new RobotOptimization(approvedOps, precedences, mutexes, forceEndTimes) // Create CP variables and (op index -> op duration maps), solve the problem and create SOPs & gantt charts.
    val roFuture = Future {
      ro.test
    }

    // For the synthesis:
    var nids = List(sopSpec) ++ zonespecs ++ uids
    var hids = nids //++ addHierarchies(nids, "hierarchy") // Todo: make and add Struct with nids

    //--------------------------------------------------------------------------------------------------------------------------
    //  run synthesis and get the optimization results

    // Run synthesis with Supremica
    val (ids2,synthAttr) = sp.virtcom.Synthesize.synthesizeModel(hids)
    val numstates =synthAttr.getAs[Int]("nbrOfStatesInSupervisor").getOrElse(-1)
    val bddName = synthAttr.getAs[String]("moduleName").getOrElse("")
    val ids_merged = hids.filter(x => !ids2.exists(y => y.id == x.id)) ++ ids2

    for {
      //Get info about the CP solution
      (cpCompl, cpTime, cpSols) <- roFuture
      sops = cpSols.map { case (makespan, sop, gantt) =>
        (makespan, SOPSpec(s"path_${makespan}", sop), gantt)
      }.sortBy(_._1)

    } yield {
      val resAttr = SPAttributes("numStates" -> numstates, "cpCompleted" -> cpCompl, "cpTime" -> cpTime, "cpSops" -> sops, "bddName" -> bddName)
     /* println("\n numstates:    " +numstates)
      println("\n cpCompl:    " +cpCompl)
      println("\n cpTime:    " +cpTime)
      println("\n bddName:    " +bddName)
      println("\n ids_merged:    " +ids_merged)
      println("\n sops.map(_._2):    " +sops.map(_._2)) */
      // Todo: return results
      //replyTo ! Response(ids_merged2 ++ sops.map(_._2), resAttr, rnr.req.service, rnr.req.reqID)
      //terminate(progress)
    } // Create a response message and send it on the bus "back to the GUI"
  }




  def extractFromSOP(individualSop :SOP, startCond : String, reallyLast : Boolean, collector : CollectorModel, ops : List[Operation], h : SPAttributes, checkedTime : Boolean) : (List[List[Operation]], String, SOP) ={
    var startCondition = startCond
    var newOpSequences = List[List[Operation]]()
    var lastNode = false
    var newSop = SOP()

    if(individualSop.isInstanceOf[Sequence]) {

      individualSop.sop.foreach(sopNode => {
        var tmpOpSequences = List[List[Operation]]()

        if(sopNode == individualSop.sop.last && reallyLast == true){ // Check if this is the last node of the sop being processed.
          lastNode = true
        }
        var (opSeqs, newStartCond, seqSop) = extractFromSOP(sopNode, startCondition , lastNode, collector,ops, h , checkedTime) // call the function again with the subNode Sop
        startCondition = newStartCond
        if(newSop.isEmpty) newSop = Sequence(List(seqSop)) else newSop.addChildren(Seq(seqSop))
        opSeqs.foreach(opSeq =>{
          if(newOpSequences.isEmpty) {tmpOpSequences :+= opSeq}
          else {newOpSequences.foreach(opsInSeq => tmpOpSequences :+= opsInSeq ++ opSeq)}
        }) // for each opSeq, append all of its sequences to the already existing ones. Create new lists as required.
        newOpSequences = tmpOpSequences
      })
    }


    else if(individualSop.isInstanceOf[OperationNode]){
      var op = ops.filter(op => op.id == individualSop.asInstanceOf[OperationNode].operation).head // get the ID from the node, find the corresponding operation

      var newId = ID.newID // Create a new ID for the op.
      val robotSchedule = op.attributes.getAs[String]("robotSchedule").getOrElse("error")
      op = op.copy(name = op.name, attributes = op.attributes merge
        SPAttributes("robotSchedule" -> robotSchedule, "original" -> op.id, "newID" -> newId),id = newId)

      startCondition = addOpToCollector (op, startCondition,reallyLast, collector,h,checkedTime) // add the operation to the collector and get the new startCondition as that ops endCond

      newOpSequences :+= List(op) // add the operation to the list
      newSop = SOP(OperationNode(newId))
    }

    else if(individualSop.isInstanceOf[sp.domain.Alternative]){
      var opSeqstmp = List[List[Operation]]()
      var newStartCondList = List[String]() // Save the end conditions from the alternatives here in a list
      var altSops = List[SOP]()
      individualSop.sop.foreach(subNode => {
        var (altSeqs, altStartCond, altSop) = extractFromSOP(subNode, startCondition , reallyLast, collector,ops, h , checkedTime) // call the function again with the subNode Sop
        newStartCondList :+= altStartCond
        altSops :+= altSop
        altSeqs.foreach(altSeq => {
          if(newOpSequences.isEmpty){opSeqstmp :+= altSeq }
          else // for each alternative, append all of its sequences to the already existing ones. Create new lists as required.
            newOpSequences.foreach(opsInSeq => opSeqstmp :+= opsInSeq ++ altSeq )
        })
      })
      startCondition = newStartCondList.mkString("", " OR ", "") // Create a new StartingConditon from the List i.e : "op1_end OR op2_end .etc...)
      newOpSequences = opSeqstmp // update the opSeq list

      if(newSop.isEmpty) newSop = SOP(Alternative(altSops)) else  newSop.addChildren(Seq(Alternative(altSops)))
    }

    (newOpSequences, startCondition, newSop) // return this
  }

  def extractFromPLCSOP(individualSop :SOP, ops : List[Operation]) : (List[List[Operation]]) ={
    var newOpSequences = List[List[Operation]]()


    if(individualSop.isInstanceOf[Sequence]) {

      individualSop.sop.foreach(sopNode => {
        var tmpOpSequences = List[List[Operation]]()

        var opSeqs = extractFromPLCSOP(sopNode,ops) // call the function again with the subNode Sop

        opSeqs.foreach(opSeq =>{
          if(newOpSequences.isEmpty) {tmpOpSequences :+= opSeq}
          else {newOpSequences.foreach(opsInSeq => tmpOpSequences :+= opsInSeq ++ opSeq)}
        }) // for each opSeq, append all of its sequences to the already existing ones. Create new lists as required.
        newOpSequences = tmpOpSequences
      })
    }

    else if(individualSop.isInstanceOf[OperationNode]){
      var op = ops.filter(op => op.attributes.getAs[ID]("original").get == individualSop.asInstanceOf[OperationNode].operation).head // get the ID from the node, find the corresponding operation
      newOpSequences :+= List(op) // add the operation to the list
    }

    else if(individualSop.isInstanceOf[sp.domain.Alternative]){
      var opSeqstmp = List[List[Operation]]()
      individualSop.sop.foreach(subNode => {
        var (altSeqs) = extractFromPLCSOP(subNode,ops) // call the function again with the subNode Sop
        altSeqs.foreach(altSeq => {
          if(newOpSequences.isEmpty){opSeqstmp :+= altSeq }
          else // for each alternative, append all of its sequences to the already existing ones. Create new lists as required.
            newOpSequences.foreach(opsInSeq => opSeqstmp :+= opsInSeq ++ altSeq )
        })
      })
      newOpSequences = opSeqstmp // update the opSeq list
    }
    (newOpSequences) // return this
  }


  def addOpToCollector (op : Operation, startcond : String, lastNode : Boolean, collector : CollectorModel, h : SPAttributes, checkedTime : Boolean) : String={
    val rs = op.attributes.getAs[String]("robotSchedule").getOrElse("error")
    val endCondition = if(!lastNode) op.name + "_done" else "idle"  // If its the last Node then the sop should be able to start over from "idle"..
    val trans = SPAttributes(collector.aResourceTrans(rs, startcond, op.name, endCondition))
    var duration =  op.attributes.getAs[Double]("duration").getOrElse(0.0)
    val spAttrDuration = if(duration > 0.0 || !checkedTime) SPAttributes("duration" -> duration) else SPAttributes("duration" -> 1.0)
    collector.opWithID(op.name, Seq(op.attributes merge trans merge h merge spAttrDuration), op.id)
    endCondition
  } // add operations to the collector, with transition conditions.

  def updateOpSequenceFromCollector(operations : List[Operation], collector : CollectorModel) : List[Operation] = { // Update the operation sequence with operations from the collector, which also contains transitions
    val collectorOps = collector.operations
    operations.flatMap(op =>  collectorOps.filter(cOps => cOps.attributes.getAs[ID]("original").get == op.attributes.getAs[ID]("original").get))
  }


  // Constraint programming (CP):
  class RobotOptimization(ops: List[Operation], precedences: List[(ID,ID)],
                          mutexes: List[(ID,ID)], forceEndTimes: List[(ID,ID)]) extends CPModel with MakeASop {

    val timeFactor = 100.0
    def test = {
      val duration = ops.map(o=>(o.attributes.getAs[Double]("duration").getOrElse(0.0) * timeFactor).round.toInt).toArray
      val indexMap = ops.map(_.id).zipWithIndex.toMap
      val numOps = ops.size
      val totalDuration = duration.sum

      // start times, end times, makespan
      var s = Array.fill(numOps)(CPIntVar(0, totalDuration))
      var e = Array.fill(numOps)(CPIntVar(0, totalDuration))
      var m = CPIntVar(0 to totalDuration)

      var extra = Array.fill(mutexes.size)(CPBoolVar())

      forceEndTimes.foreach { case (t1,t2) => add(e(indexMap(t1)) == s(indexMap(t2))) }

      precedences.foreach { case (t1,t2) => add(e(indexMap(t1)) <= s(indexMap(t2))) }
      mutexes.zip(extra).foreach { case ((t1,t2),ext) =>
        val leq1 = e(indexMap(t1)) <== s(indexMap(t2))
        val leq2 = e(indexMap(t2)) <== s(indexMap(t1))
        add(leq1 || leq2)

        // extra
        add(!ext ==> leq1)
        add(leq1 ==> !ext)
        add(ext ==> leq2)
        add(leq2 ==> ext)
      }
      ops.foreach { op =>
        // except for time 0, operations can only start when something finishes
        // must exist a better way to write this
        add(e(indexMap(op.id)) == s(indexMap(op.id)) + duration(indexMap(op.id)))
        val c = CPIntVar(0, numOps)
        add(countEq(c, e, s(indexMap(op.id))))
        // NOTE: only works when all tasks have a duration>0
        add(s(indexMap(op.id)) === 0 || (c >>= 0))
      }
      add(maximum(e, m))
      minimize(m)
      search(binaryFirstFail(extra++s++Array(m)))

      var sols = Map[Int, Int]()
      var ss = Map[Int,List[(ID,Int,Int)]]()
      onSolution {
        sols += m.value -> (sols.get(m.value).getOrElse(0) + 1)
        /*
              ops.foreach { op =>
                println(op.name + ": " + s(indexMap(op.id)).value + " - " +
                 duration(indexMap(op.id)) + " --> " + e(indexMap(op.id)).value)
              } */
        // sols.foreach { case (k,v) => println(k + ": " + v + " solutions") }
        val ns = ops.map { op => (op.id, s(indexMap(op.id)).value,e(indexMap(op.id)).value) }
        ss += m.value->ns
      }

      val stats = start(timeLimit = 120) // (nSols =1, timeLimit = 60)

      val sops = ss.map { case (makespan, xs) =>
        val start = xs.map(x=>(x._1,x._2)).toMap
        val finish = xs.map(x=>(x._1,x._3)).toMap

        def rel(op1: ID,op2: ID): SOP = {
          if(finish(op1) <= start(op2))
            Sequence(List(SOP(op1),SOP(op2)))
          else if(finish(op2) <= start(op1))
            Sequence(List(SOP(op2),SOP(op1)))
          else
            Parallel(List(SOP(op1),SOP(op2)))
        }

        val pairs = (for {
          op1 <- ops
          op2 <- ops if(op1 != op2)
        } yield Set(op1.id,op2.id)).toSet

        val rels = pairs.map { x => (x -> rel(x.toList(0),x.toList(1))) }.toMap

        val opsPerRob = ops.groupBy(_.attributes.getAs[String]("robotSchedule")).collect {
          case (Some(s), op) => s -> op
        }.map { case (k,v) => //println("schedule " + k + " contains " + v.map(x=>x.name+" "+x.id).mkString(", "))
          v.map(_.id) }.toList

        val sop = opsPerRob.map(l=>makeTheSop(l, rels, EmptySOP)).flatten

        (makespan/timeFactor, sop, xs.map(x=>(x._1,x._2/timeFactor,x._3/timeFactor)))
      }
      (stats.completed, stats.time, sops.toList)
    }
  }


}
