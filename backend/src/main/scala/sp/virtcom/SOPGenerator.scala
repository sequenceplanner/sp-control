package sp.virtcom

import sp.domain._
import sp.domain.Logic._
import sp.patrikmodel.CollectorModel


object SOPGenerator extends SOPGen
trait SOPGen {

  def generateSOPs(selectedSchedules : Set[ID], ids : List[IDAble]): List[IDAble with Product with Serializable] ={

    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation]) // filter out the operations from the ids
    val schedules = ops.filter(op => selectedSchedules.contains(op.id)) // Get the selected operations as a list

    val structs = ids.filter(_.isInstanceOf[Struct]).map(_.asInstanceOf[Struct])
    val activeStruct = structs.find(struct => struct.items.map(sn => sn.item).contains(schedules.head.id)).get // Get the struct which the first schedule is a part of. I assume here that the other schedules are in the same Struct for now

    val scheduleNames = schedules.map(op =>
      if(op.attributes.getAs[List[String]]("robotcommands").getOrElse(List()).nonEmpty){
        op.name
        //ops.find(o => o.id ==activeStruct.items.find(sn => sn.item == op.id).get.parent.get).get.name // find the parent operation of the robot schedule, (the actual schedule name is not unique, could just add something else instead)
      }
      else op.name
    ).toSet.mkString("_") // Create Schedule names, for robots schedules and other resources.

    val h = SPAttributes("hierarchy" -> Set("VRS_"+ scheduleNames ))  // This will be the name of the new hierarchy...


    case class VolvoRobotScheduleCollector(val modelName: String = "VolvoRobotSchedule") extends CollectorModel
    val collector = VolvoRobotScheduleCollector() // This can collect all of the operations and create transition variables between them
    val idle = "idle" // This is a value for the variables created in the collector, and it should later also be assigned to the first operation(s) in a schedule (SOP)

    val zoneMapsAndOps = schedules.map { schedule => // go through each schedule ( selected op)
      val robcmds = schedule.attributes.getAs[List[String]]("robotcommands").getOrElse(List()) // Gets all of the robotcommands for the selected operation/schedule


    val opNode = activeStruct.items.find(sn =>sn.item == schedule.id).get // Get opNode of schedule
    val Node = if(robcmds.isEmpty) opNode else activeStruct.items.find(sn => sn.nodeID == opNode.parent.get).get // get parent of opNode
    val scheduleOps = activeStruct.getAllChildren(Node).map(sn => ops.find(o => o.id == sn.item).get) // Get all operations that are relevant for this schedule

      var rs = if(robcmds.isEmpty){"LD" + schedule.name} else {ops.find(_.id == Node.item).get.name}    // The name of the Selected operation/schedule
      collector.v(robotScheduleVariable(rs), idleValue = Some(idle), attributes = h) //  Update the collector to gather all of the variables and operations from the functions below

      if(robcmds.nonEmpty)
        addRobot(Map[String, List[List[Operation]]](), Map[String, Seq[SOP]](), Set[String](), robcmds, scheduleOps, rs, activeStruct, ops, collector, h) // Get Sops, Ops and zones from robot schedule
      else{
        val rSOP= addOtherResource(scheduleOps: List[Operation], rs: String, h: SPAttributes, collector: CollectorModel)
        (Map[String, List[List[Operation]]](),Set[String](), List(rSOP), Map[String, Seq[SOP]]()) // Just so that the result will conform with the addRobot function
      }
    }
    val operations = collector.operations // Get all operations from the collector
    val sops = zoneMapsAndOps.map { x => x._3 }.flatten // extract SOPS
    val sopspec = SOPSpec(schedules.map(_.name).toSet.mkString("_") + "_Original_SOP", sops, h) // Create a Sop Spec with (name, sops, belonging to hierarchy)


    var allZoneSopMapsMerged =  Map[String, Seq[SOP]]() // Merges all of the key values in the zone sop maps
    zoneMapsAndOps.map { zoneSopMap => zoneSopMap._4.map { case (z, sopSeq) =>  if(allZoneSopMapsMerged.contains(z)) allZoneSopMapsMerged += z-> (allZoneSopMapsMerged(z) ++  sopSeq )   else allZoneSopMapsMerged += z-> sopSeq }}
    val zonespecs = allZoneSopMapsMerged.map { case (z, l) => SOPSpec(z, List((Arbitrary(l.toList))), h) } // Creates arbitrary SOPs for each Zone and SopSpecs for them


    val plcSOP = List(SOPSpec("PLC_SOP", List[SOP](), h)) // Create an empty sopSpec, that the user can fill in to model the PLC and connections between different operations from different resources.

    val nids = List(sopspec) ++ zonespecs ++ plcSOP ++ operations // new ids, i.e everything that we want to return

    var snids =List(StructNode(sopspec.id)) ++ zonespecs.map( z => StructNode(z.id)).toList ++ plcSOP.map(p => StructNode(p.id))  ++ operations.map(o => StructNode(o.id))

    nids :+ Struct("VRS_"+ scheduleNames, snids.toSet)
  }

  def robotScheduleVariable(rs: String) = "v"+rs+"_pos"

  def addOtherResource(rOps: List[Operation], rs: String, h: SPAttributes, collector: CollectorModel): SOP ={

    var activeOps = List[Operation]() // this will contain a list of the operations that are actually supposed to be executed, in that order, It is used to create a SOP
    // Todo : The functions used for the formal synthesis do not handle "-" symbols. So either change those functions or change the operation names here. It would be best to change the functions.
    // This could be defined somewhere else or sent from the GUI. It could be modified to create parallel sequences by adding a list which the nextstates Array can reside in and then loop over that list adding the Sequences as alternatives if desired.
    var currentState = "HOME" // The starting state, which will then be updated with current state, i.e next state in states
    val states = Array("OpenSeq2", "CloseSeq1","CloseSeq2","CloseSeq3","CloseSeq4","CloseSeq5","OpenSeq1","OpenSeq2")

    states.foreach(state =>
    {
      var foundOp = rOps.find(op=> op.attributes.getAs[String]("source_pose").getOrElse("") == currentState && op.attributes.getAs[String]("target_pose").getOrElse("")   == state) // In each device operation there exists a source pose and a target pose, check which operation poses corresponds to the current and next State.
      if(foundOp.nonEmpty) {
        val newId = ID.newID // Create a new ID to make a copy of the operation
        val newOp = foundOp.get.copy(name = rs + "_" + foundOp.get.name.replace('-', '_'), attributes = foundOp.get.attributes merge  // There seems to be a problem with having "-" chars in the synthesis, have not found where... Which is why I change it here
          SPAttributes("robotSchedule" -> rs, "original" -> foundOp.get.id),id = newId)
        collector.opWithID(newOp.name, Seq(newOp.attributes merge h),newId) // Add the operation to the Collector
        activeOps :+= newOp
        currentState = state}})

    Sequence(activeOps.map(op=> SOP(op.id)))
  }





  // Creates a SOP and zone SOP map, given a robot schedule and some operations and stuff.
  def addRobot(zoneMap: Map[String, List[List[Operation]]], zoneSopMapping :  Map[String, Seq[SOP]],
               activeZones: Set[String], robotCommands : List[String],
               availableOps: List[Operation],
               robotSchedule: String, activeStruct: Struct, ops: List[Operation],
               collector: CollectorModel, h: SPAttributes): (Map[String, List[List[Operation]]], Set[String], List[SOP], Map[String, Seq[SOP]]) = {


    var zMap = zoneMap
    var activeZoneSet = activeZones
    var availableOperations = availableOps
    var zoneSopMap = zoneSopMapping

    var ss = List[SOP]() // The SOP that will eventually be sent back to the GUI for display.

    robotCommands.foreach(command => { // Evaluate each robot command

      // Active Zones
      if (command.startsWith("WaitSignal AllocateZone")) {
        val zoneIndex = command.indexOf("Zone")  // Gets the index of where the String Zone starts of the robot command
        val zoneStr = cleanName(command.substring(zoneIndex), true) // removes other parts of the command not containing the name of the zone
        activeZoneSet += zoneStr // Adds the zone to a set of active Zones, being a set it can only contain one instance of the zone.
      }

      else if (command.startsWith("WaitSignal ReleaseZone")) {

        val zoneIndex = command.indexOf("Zone") // Get the index of the word zone in the command string
        val zoneStr = cleanName(command.substring(zoneIndex), true) // remove things around the string
        activeZoneSet -= zoneStr // the zone is no longer active, remove it from the set

        if(zMap.contains(zoneStr)) {
          val zoneSequences = zMap(zoneStr).map(opList =>  Sequence(opList.map(op => SOP(op)))).toSeq // Goes through the operation lists, and for each operation creates a Hierarchy. The Hierarchies are placed in list in Sequences which are also in a list, but transformed to a Seq.
          zMap -= zoneStr // removes the entry of this zone in the zone map
          if(zoneSopMap.contains(zoneStr)) zoneSopMap += zoneStr -> (zoneSopMap(zoneStr) ++ zoneSequences) else  zoneSopMap += zoneStr ->  zoneSequences // add the Zone sequence SOP to the zone sop map
        }
      }


      else if (command.startsWith("WaitCase")) { // Check if any of the "Commands" contains a string with a Case statement.

        val CaseSeparatedInAlphaNumericalList = command.split("\\W+")  // Take the case statement and split it into parts of alphanumerical symbols
        var allCaseSopSeqs = List[List[SOP]]()
        var zMapTmpList = List[Map[String,List[List[Operation]]]]() // Creates a new List for the Zon maps where all the operations from the Cases will be saved
        var activeZoneSetNew = Set[String]() // The active zones after the Cases will have changed, so here is a new Set
        CaseSeparatedInAlphaNumericalList.foreach(caseString => // Go through each case part and check if the part exists as an operation in the availableOps tree.
          availableOperations.find(caseOp => caseOp.name == caseString) // find an operation with the same name as the robot command's case
          match {
            case Some(caseOp) => // If there is an operation that matches the case

              val opNode = activeStruct.items.find(sn =>sn.item == caseOp.id).get
              val operationChildOps = activeStruct.getChildren(opNode).map(sn => ops.find(o => o.id == sn.item).get) // Get the kids of the Case operation
            var robotCommandsInChild = List[String]() // Init a new robot command list, to use in recursion of this function
            var newOp = caseOp // Init a new Operation, could be anything just needs the correct type here.
            var zMaptmp = Map[String, List[List[Operation]]]() // This is the place zones and operations will be mapped for each new case.
              operationChildOps.foreach(opChild => {
                if (opChild.name == caseString) { // The first operation within the case has the same name as the actual Case, and may contain a robot schedule, at least for the PS model in this project
                  robotCommandsInChild = opChild.attributes.getAs[List[String]]("robotcommands").getOrElse(List()) // Get the robot commands as a list of strings, if there are none, then empty list.
                  val newId = ID.newID // Create a new ID for the operation, so that it can be saved in a new hierarchy separate from the old one.
                  newOp = opChild.copy(name = robotSchedule + "_" + opChild.name, attributes = opChild.attributes merge // Copy the operation with new, name, ID and attributes
                    SPAttributes("robotSchedule" -> robotSchedule, "original" -> opChild.id), id = newId)

                  activeZoneSet.foreach(z => {
                    zMaptmp += z -> List(List(newOp))
                  }) // Add the operation to the Zone map, checking which zones are active
                  collector.opWithID(newOp.name, Seq(newOp.attributes merge h),newId) // Save the operation in the collector
                  if (robotCommandsInChild.isEmpty) {
                    zMapTmpList :+= zMaptmp
                    activeZoneSetNew = activeZoneSetNew.union(activeZoneSet) // If there is a case with no operations following, then all the zones that were active before can be active afterwards too.
                  } // adds the zone map with only one operation to the list of maps in case there are no robot commands, since then there will be no further operations executing in this case.
                }
                availableOperations :+= opChild // Add the child operation of the case to the list of available operations.
              })

              var caseSopSeqs = List[SOP]()
              if (robotCommandsInChild.nonEmpty) { // If there are robotcommands this might mean that there are more operations that can be executed and added. If so recursion will be utilized to get SOP and Zone information about them.
                val (zMapCaseTmp, activeZonesTmp, sopSeqsTmp, zSopMapTmp) = addRobot(zMaptmp, zoneSopMap, activeZoneSet, robotCommandsInChild, availableOperations, robotSchedule, activeStruct, ops, collector, h) // call the function again
                zMapTmpList :+= zMapCaseTmp // Adds the zonemap of the Case to the Zonemap list
                activeZoneSetNew = activeZoneSetNew.union(activeZonesTmp) // Adds the active zones of the Case
                caseSopSeqs = sopSeqsTmp // Gets the SOP from the Case
                zoneSopMap = zSopMapTmp // Gets the zone SOP from the Case
              }
              caseSopSeqs = if (caseSopSeqs.nonEmpty) List(Sequence(List(SOP(newOp)) ++ caseSopSeqs(0).sop)) else List(Sequence(List(SOP(newOp)))) // add the initial Case operation to the start of the SOP
              allCaseSopSeqs :+= caseSopSeqs // Add the Case SOP to the list of case SOPs
            case none => // do nothing
          })
        activeZoneSet = activeZoneSetNew // Updates the active zones with the active zones of the cases.

        var zMapNew = Map[String,List[List[Operation]]]() //Creating a new ZoneMap from the Cases zonemaps
        zMapTmpList.foreach(zM => zM.keys.foreach(z => { // Go through each of the zonemaps, get the keys for a zonemap, for every key
          var newOpLists = List[List[Operation]]() // Create a new list to add lists of operations to
          if (zMap.contains(z)) // Check if the original zone map contains a key of one of the new zone maps
            newOpLists= zMap(z).map(opList => { zM(z).map(zMOplist => opList ++ zMOplist)}).flatten // if it does, then I want to append all of the new Op lists to the end of every old one.
          else newOpLists= zM(z) // Otherwise it is only necessary to add the new list of op lists

          if(zMapNew.contains(z)) zMapNew += z -> (zMapNew(z) ++ newOpLists ) else zMapNew += z -> newOpLists // Then add this to the new zone map and repeat.
        }))
        zMap = zMapNew // Update the zone map

        val caseSeqOfSops = allCaseSopSeqs.filterNot(sopList => sopList.isEmpty).map(sopList => Sequence(sopList(0).sop)) // removes empty sop lists, then maps each sop to a sequence instead

        if (caseSeqOfSops.nonEmpty) {
          var AlternativeSops = Alternative(caseSeqOfSops)  // Creates alternatives out of the seq[Sop]
          ss = List(if (!ss.isEmpty) Sequence((ss(0).sop :+ AlternativeSops)) else Sequence(List[SOP]() :+ AlternativeSops)) // adds the alternative Sop to the SOP that will be returned by the function
        }
      }
      else if (command.startsWith("!")) {
      } // do nothing, because that means the line of the robot schedule is commented
      else {
        val cleanOpName = cleanName(command, false)
        availableOps.find(operation => operation.name == cleanOpName) match {
          // find the current command in the list of available operations.
          case Some(operation) =>
            val newId = ID.newID // Create a new ID for the operation
          val newOp = operation.copy(name = robotSchedule + "_" + operation.name, attributes = operation.attributes merge
            SPAttributes("robotSchedule" -> robotSchedule, "original" -> operation.id),id = newId) // Create a new operation out of the old one, with new ID, name and attributes

            activeZoneSet.foreach(z => {if(zMap.contains(z)) zMap += z -> (zMap(z).map(opList => opList :+ newOp) ) else zMap += z ->  List(List(newOp)) } ) // adds the operation to the end of all existing operation lists mapped to the active zones
            collector.opWithID(newOp.name, Seq(newOp.attributes merge h),newId) // adds the operation to the collector

            ss = List(if (!ss.isEmpty) Sequence((ss(0).sop :+ SOP(newOp))) else Sequence(List(SOP(newOp))) ) // Adds the operation to the SOP that will later be sent back from the function
          case none =>
        }
      }
    })
    (zMap, activeZoneSet, ss, zoneSopMap) // return zone map, active zones, SOP, and zone SOP map.
  }

  def cleanName(str: String, rmvSlash : Boolean): String = {

    val s = if(!str.startsWith("''  -  '")) str else {
      val ns = str.substring(8,str.length)
      val p = ns.indexOf("'")
      if(p == -1) ns else ns.substring(0,p)
    }

    val pos = s.indexOf(";")
    val cleanStr = if(pos < 0) s else s.substring(0,pos)

    if (rmvSlash)
      cleanStr.split("""\\""")(0) // remove everything with and after backslash \
    else
      cleanStr
  }
}
