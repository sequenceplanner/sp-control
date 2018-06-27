package spgui.widgets.VDGUI

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import play.api.libs.json.JsString
import scalacss.internal.StyleA
import sp.devicehandler.VD.{OneToOneMapper, Resource}
import sp.domain._
import spgui.SimpleSet
import spgui.circuits.main.handlers.DriverHandler.DriverId
import spgui.circuits.main.handlers.DriverInfo
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.widgets.VDGUI.SPCardComponent.ResourceCard

/** Widget to visualize the Resources and it's status*/
object ResourceWidget {
  case class Props(proxy: ModelProxy[FrontendState]) {
    val drivers: SimpleSet[DriverId, DriverInfo] = proxy.value.drivers.drivers
  }

  private class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      val resources = props.proxy.value.virtualDevices.virtualDevices.flatMap(_.resources.toList).toList
      val cards = resources.map(data => renderResourceCard(props, data.resource, data.state)).sortBy(_.name)

      SPCardComponent(cards)
    }

    def renderResourceCard(props: Props, resource: Resource, resourceState: Map[ID, SPValue]): ResourceCard = {
      val oneToOneMappers = resource.stateMap.collect { case x: OneToOneMapper => x }

      val driverStatuses = props.drivers
        .filterKeys(oneToOneMappers.map(_.driverID).contains)
        .map(driver => (driver.name, driver.status))
        .toList

      // val nameValueTuples = oneToOneMappers.map(m => (m.driverIdentifier.toString, resourceState(m.thing)))
      val nameValueTuples = oneToOneMappers
          .map(m => (m.driverIdentifier.toString, resourceState.getOrElse(m.thing, JsString("NULL"))))

      SPCardComponent.ResourceCard(resource.id, resource.name, driverStatuses, nameValueTuples)
    }
  }

  private val resourceWidgetComponent = ScalaComponent.builder[Props]("ResourceWidget")
    .renderBackend[Backend]
    .build

  implicit def toHtml(a: StyleA): TagMod = ^.className := a.htmlClass

  private val connectComponent = MainCircuit.connectComponent(identity)

  def apply() = spgui.SPWidget(_ => connectComponent { proxy => resourceWidgetComponent(Props(proxy)) })
}
