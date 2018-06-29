package spgui.widgets.VDGUI

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import play.api.libs.json.{JsString, JsValue}
import sp.devicehandler.VD.{OneToOneMapper, Resource}
import sp.domain._
import spgui.SimpleSet
import spgui.circuits.main.handlers.Aliases.DriverName
import spgui.circuits.main.handlers.DriverHandler.{DriverId, DriverStatus}
import spgui.circuits.main.handlers.DriverInfo
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.widgets.VDGUI.cards.ResourceView
import spgui.widgets.VDGUI.cards.ResourceView.ResourceCard
import spgui.widgets.VDGUI.cards.{CardViewCSS => css}
import CSSHelpers.toHtml



/**
  * Widget to visualize the Resources and it's status
  */
object ResourceWidget {
  implicit val simpleSetReusability: Reusability[DriverInfo] = Reusability.by_==
  implicit val propsReusability: Reusability[Props] = Reusability.by(_.drivers.toList)

  case class Props(proxy: ModelProxy[FrontendState]) {
    val drivers: SimpleSet[DriverId, DriverInfo] = proxy.value.drivers.drivers
    val items: SimpleSet[ID, IDAble] = proxy.value.models.activeModel.map(_.items).getOrElse(SimpleSet[ID, IDAble](_.id))
  }
  private class Backend($: BackendScope[Props, Option[ID]]) {
    def render(props: Props, expandedId: Option[ID]): VdomElement = {
      val resources = props.proxy.value.virtualDevices.virtualDevices.flatMap(_.resources.toList).toList
      val cards = resources.map(data => renderResourceCard(props, data.resource, data.state)).sortBy(_.name)

      val expandedCard = expandedId.flatMap(id => cards.find(_.cardId == id))
      expandedCard match {
        case Some(card) =>
          ResourceView.Detail(card, onClick = $.setState(None))
        case None => ResourceView.Overview(cards, onCardClick = cardId => $.setState(Some(cardId)))
      }
    }

    def renderResourceCard(props: Props, resource: Resource, resourceState: Map[ID, SPValue]): ResourceCard = {
      val associations = resource.stateMap.collect { case x: OneToOneMapper => x }

      val driverStatuses = props.drivers
        .filterKeys(associations.map(_.driverID).contains)
        .map(driver => (driver.name, driver.status))
        .toList


      val nameValueTuples = {
        val fromResourceState = resourceState.flatMap { case (id, v) =>
          props.items.get(id).map(idAble => (id, idAble.name, v))
        }

        val fromAssociations = associations.map { case OneToOneMapper(thingId, _, name) =>
          val value = resourceState.getOrElse(thingId, JsString("NULL"))
          (thingId, name, value)
        }

        (fromResourceState ++ fromAssociations)
          .distinctBy { case (id, _, _) => id }
          .toList
          .sortBy { case (_, name, _) => name }
          .map { case (_, name, value) => (name, value) }
      }

      ResourceCard(resource.id, resource.name, driverStatuses, nameValueTuples)
    }

  }

  implicit class DistinctIterable[V](xs: Iterable[V]) {
    def distinctBy[K](f: V => K): Iterable[V] = xs.groupBy(f).map { case (_, v) => v.head }
  }

  def driverStatus(status: String): TagMod = {
    val driverCss: TagMod = status match {
      case DriverStatus.Online => css.driver.online
      case DriverStatus.Offline => css.driver.offline
      case DriverStatus.Unresponsive => css.driver.unresponsive
      case DriverStatus.Terminated => css.driver.terminated
      case _ => EmptyVdom
    }

    <.span(driverCss, status)
  }

  def renderDriverStatus(statuses: List[(DriverName, DriverStatus)]): TagMod = {
    statuses.map { case (name, status) =>
      <.div(css.driverStatus, <.span(css.driverName, name), driverStatus(status))
    }.toTagMod
  }

  val jsValueToString: PartialFunction[JsValue, String] = {
    case JsString(v) => v
    case x => x.toString()
  }

  private val resourceWidgetComponent = ScalaComponent.builder[Props]("ResourceWidget")
    .initialState(Option.empty[ID])
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

  private val connectComponent = MainCircuit.connectComponent(identity)

  def apply() = spgui.SPWidget(_ => connectComponent { proxy => resourceWidgetComponent(Props(proxy)) })
}
