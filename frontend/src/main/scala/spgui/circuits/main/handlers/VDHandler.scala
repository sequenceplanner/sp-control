package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.devicehandler.VD.{DriverStateMapper, DriverWithState, Resource, ResourceWithState}
import sp.domain.{ID, SPAttributes, SPValue}
import spgui.SimpleSet
import spgui.circuits.main.handlers.Aliases._
import spgui.circuits.main.handlers.DriverHandler.DriverId

trait VDAction extends Action
case class AddVirtualDevice(name: String, id: VirtualDeviceId, resources: Iterable[ResourceWithState], drivers: Iterable[DriverWithState], attributes: SPAttributes) extends VDAction
case class UpdateResource(resourceId: ResourceId, state: Map[ID, SPValue], diff: Boolean = false) extends VDAction
case class ModelNames(names: List[VDModelName]) extends VDAction
case class RunnerCreated(id: RunnerId) extends VDAction
case object TerminateAllVirtualDevices extends VDAction

@Lenses case class VDHandlerState(
                                   virtualDevices: SimpleSet[VirtualDeviceId, VDData],
                                   availableVDModels: List[VDModelName],
                                   latestActiveRunnerId: Option[RunnerId]
                                 )

object VDHandlerState {
  def device(id: VirtualDeviceId) = virtualDevices ^|-? SimpleSet.at[VirtualDeviceId, VDData](id)
  def resource(id: ResourceId) = VDData.resources ^|-? SimpleSet.at[ResourceId, ResourceData](id)
}

class VDHandler[M](modelRW: ModelRW[M, VDHandlerState]) extends StateHandler[M, VDHandlerState, VDAction](modelRW) {
  import VDHandlerState.{virtualDevices, resource, device, availableVDModels, latestActiveRunnerId}

  override def onAction: PartialFunction[VDAction, Reaction] = {
    case props: AddVirtualDevice =>
      //println("AddVirtualDevice")
      //props.resources.map(data => ResourceData(data.resource, data.state)).foreach(println)
      // Assumes that if there is a VD with the same ID, it should be replaced with the incoming one.
      virtualDevices.modify(_ + createDevice(props))

    case props: UpdateResource =>
      react {
        value.virtualDevices
          .find(_.resources.contains(props.resourceId))
          .map(vd => (device(vd.id) ^|-? resource(props.resourceId)).modify(_.copy(state = props.state)))
          .getOrElse(identity)
      }

    case ModelNames(names) =>
      availableVDModels.set(names)

    case RunnerCreated(id) =>
      latestActiveRunnerId.set(Some(id))

    case TerminateAllVirtualDevices =>
      println("TerminateAllVirtualDevices")
      react {
      virtualDevices.set(SimpleSet[VirtualDeviceId, VDData](_.id)) compose latestActiveRunnerId.set(None)
    }
  }

  def createDevice(props: AddVirtualDevice): VDData = {
    val resourceData = props.resources.map(data => ResourceData(data.resource, data.state)).toSeq
    val driverData = props.drivers.map(data => DriverInfo(data.d, data.state)).toSeq

    VDData(
      name = props.name,
      id = props.id,
      resources = SimpleSet(_.id, resourceData:_*),
      drivers = SimpleSet(_.id, driverData:_*),
      attributes = props.attributes
    )
  }

  override def acceptAction: Action => Boolean = {
    case _: VDAction => true
    case _ => false
  }
}

@Lenses case class VDData(
                           name: String,
                           id: VirtualDeviceId,
                           resources: SimpleSet[ResourceId, ResourceData],
                           drivers: SimpleSet[DriverId, DriverInfo],
                           attributes: SPAttributes
                         )

case class ResourceData(resource: Resource, state: Map[ID, SPValue]) {
  val name: String = resource.name
  val id: ID = resource.id
  val things: Set[ID] = resource.things
  val stateMap: List[DriverStateMapper] = resource.stateMap
  val setup: SPAttributes = resource.setup
  val sendOnlyDiffs: Boolean = resource.sendOnlyDiffs
}

object VDHandler {
  val initialState: VDHandlerState = VDHandlerState(new SimpleSet(_.id, Map()), List(), latestActiveRunnerId = None)
}