package spgui.widgets.VDGUI
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import play.api.libs.json.JsSuccess
import sp.domain._
import spgui.communication._
import sendMessages._
import sp.devicehandler.VD.Driver
import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}


object SPCardGrid {
  case class State(expandedId: Option[ID] = None)
  case class Props(cards: List[RenderCard])

  trait RenderCard{val cardId: ID}
  case class DriverCard(cardId: ID, name: String, status: String, typ: String, setup : SPAttributes, state: Map[String, SPValue]) extends RenderCard
  case class ResourceCard(cardId: ID, name: String, driverStatuses: List[(String, String)], state: List[(String, SPValue)]) extends RenderCard


  class Backend($: BackendScope[Props, State]) {
    def render(p:Props, s: State) = {
      val isExpanded = !s.expandedId.isEmpty
      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        <.div(
          { isExpanded match {
            case true  => ^.className := DriverWidgetCSS.cardGroupExpanded.htmlClass
            case false => ^.className := DriverWidgetCSS.cardGroupCollapsed.htmlClass
          }},
          p.cards.map(
            c => c match {
              case dc: DriverCard => {
                val smallCard = driverCardSmall(dc)
                val expandedCard = driverCardExpanded(dc)
                renderCard(dc.cardId, s.expandedId, expandedCard, smallCard)
              }
              case rc: ResourceCard => {
                val smallCard = resourceCardSmall(rc)
                val expandedCard = resourceCardExpanded(rc)
                renderCard(rc.cardId, s.expandedId, expandedCard, smallCard)
              }
            }
          ).toTagMod
        )
      )
    }

    def renderCard(
      cardId: ID,
      expandedId: Option[ID],
      cardContentsExpanded: TagMod,
      cardContentsCollapsed: TagMod
    ): TagMod = {
      val isExpanded = expandedId == Some(cardId)
      List(
        <.span(
          ^.className := DriverWidgetCSS.cardPlaceholder.htmlClass,
          expandedId match {
            case None => EmptyVdom
            case _ =>
              if(expandedId == Some(cardId)) ^.className := DriverWidgetCSS.cardExpanded.htmlClass
              else ^.className := DriverWidgetCSS.cardCollapsed.htmlClass
          },
          <.span(
            ^.className := DriverWidgetCSS.cardOuter.htmlClass,
            expandedId match {
              case None => EmptyVdom
              case _ =>
                if(expandedId == Some(cardId)) ^.className := DriverWidgetCSS.unsetHeight.htmlClass
                else EmptyVdom
            },
            {
              isExpanded match {
                case true => cardContentsExpanded
                case false => cardContentsCollapsed
              }
            }
          )
        )
      ).toTagMod
    }


    def driverCardSmall(card: DriverCard) = {
      <.div(
        ^.className := DriverWidgetCSS.driverCard.htmlClass,
        <.div(
          ^.className := DriverWidgetCSS.cardTitleSmall.htmlClass,
          ^.onClick --> $.modState(s =>
            if(s.expandedId == Some(card.cardId)) s.copy(expandedId = None)
            else s.copy(expandedId = Some(card.cardId))
          ),
          card.name
        ),
        <.div(
          ^.className := DriverWidgetCSS.driverTypeSmall.htmlClass,
          <.div("Type: " + card.typ)
        ),
        <.div(
          ^.className := DriverWidgetCSS.driverStatusSmall.htmlClass,
          <.span("Status: "),
          <.span(
            card.status match {
              case "Online" => <.span(
                ^.className := DriverWidgetCSS.driverOnline.htmlClass,
                "Online"
              )
              case "Offline" => <.span(
                ^.className := DriverWidgetCSS.driverOffline.htmlClass,
                "Offline"
              )
              case "Unresponsive" => <.span(
                ^.className := DriverWidgetCSS.driverUnresponsive.htmlClass,
                "Unresponsive"
              )
              case s: String => <.span(s)
            }
          )
        )
      )
    }
    def driverCardExpanded(card: DriverCard) = {
      <.div(
        ^.className := DriverWidgetCSS.driverCard.htmlClass,
        <.div(
          ^.className := DriverWidgetCSS.cardTitleExpanded.htmlClass,
          ^.onClick --> $.modState(s =>
            if(s.expandedId == Some(card.cardId)) s.copy(expandedId = None)
            else s.copy(expandedId = Some(card.cardId))
          ),
          card.name
        ),
        <.div(
          ^.className := DriverWidgetCSS.driverType.htmlClass,
          <.div("Type: " + card.typ)
        ),
        <.div(
          ^.className := DriverWidgetCSS.driverStatus.htmlClass,
          <.span("Status: "),
          <.span(
            card.status match {
              case "Online" => <.span(
                ^.className := DriverWidgetCSS.driverOnline.htmlClass,
                "Online"
              )
              case "Offline" => <.span(
                ^.className := DriverWidgetCSS.driverOffline.htmlClass,
                "Offline"
              )
              case "Unresponsive" => <.span(
                ^.className := DriverWidgetCSS.driverUnresponsive.htmlClass,
                "Unresponsive"
              )
              case s: String => <.span(s)
            }
          )
        ),
        <.div(
          ^.className := DriverWidgetCSS.driverStates.htmlClass,
          <.table(
            ^.className :=DriverWidgetCSS.table.htmlClass,
            <.tbody(
          card.state.map( s => {
            <.tr(
              <.td(s._1),
              <.td(s._2.toString()),  // Todo: a dropdown for boolean?
              <.td(<.input(^.placeholder := "Change value...", ^.onKeyPress ==> { updateDriverState(card, s._1)}, ^.className := DriverWidgetCSS.input.htmlClass))
            )
          }).toTagMod
            ))
        ),
        <.button(^.className := "btn", ^.onClick --> sendToDeviceDriver(APIDeviceDriver.TerminateDriver(card.cardId)), "Terminate Driver"),
        <.button(^.className := "btn", ^.onClick --> sendToDeviceDriver(APIDeviceDriver.SetUpDeviceDriver(Driver(card.name, card.cardId, card.typ, card.setup))), "Start Driver")
      )
    }

    def createCorrectTypeOfSPValue(sPValue: SPValue, newValue : String) : SPValue =  { // Convert the incoming string to an SPvalue of the same type as the previous state value
      if (sPValue.validate[Int].isSuccess)          {SPValue(newValue.toInt)}
      else if(sPValue.validate[Boolean].isSuccess)  {SPValue(newValue.toBoolean)}
      else                                          {SPValue(newValue)}
    }

    def updateDriverState(card: DriverCard, s1 : String)(e: ReactKeyboardEventFromInput) = {
      if(e.key == "Enter") {
        val newState = (card.state + (s1 -> createCorrectTypeOfSPValue(card.state(s1), e.target.value ) ) )
        sendToDeviceDriver(APIDeviceDriver.DriverCommand(card.cardId, newState) )
      }
      else
        Callback.empty
    }


    def resourceCardSmall(card: ResourceCard) = {
      <.div(
        ^.className := DriverWidgetCSS.resourceCard.htmlClass,
        <.div(
          ^.className := DriverWidgetCSS.cardTitleSmall.htmlClass,
          ^.onClick --> $.modState(s =>
            if(s.expandedId == Some(card.cardId)) s.copy(expandedId = None)
            else s.copy(expandedId = Some(card.cardId))
          ),
          card.name
        ),
        {
          card.driverStatuses.map(driverStatus =>
            <.div(
              ^.className := DriverWidgetCSS.driverStatusSmall.htmlClass,
              <.span(driverStatus._1 + ": "),
              <.span(
                driverStatus._2 match {
                  case "Online" => <.span(
                    ^.className := DriverWidgetCSS.driverOnline.htmlClass,
                    "Online"
                  )
                  case "Offline" => <.span(
                    ^.className := DriverWidgetCSS.driverOffline.htmlClass,
                    "Offline"
                  )
                  case "Unresponsive" => <.span(
                    ^.className := DriverWidgetCSS.driverUnresponsive.htmlClass,
                    "Unresponsive"
                  )
                  case s: String => <.span(s)
                }
              )
            )
          )
        }.toTagMod
      )
    }

    def resourceCardExpanded(card: ResourceCard) = {
      <.div(
        ^.className := DriverWidgetCSS.resourceCard.htmlClass,
        <.div(
          ^.className := DriverWidgetCSS.cardTitleExpanded.htmlClass,
          ^.onClick --> $.modState(s =>
            if(s.expandedId == Some(card.cardId)) s.copy(expandedId = None)
            else s.copy(expandedId = Some(card.cardId))
          ),
          card.name
        ),
        {
          card.driverStatuses.map(driverStatus =>
            <.div(
              ^.className := DriverWidgetCSS.driverStatus.htmlClass,
              <.span(driverStatus._1 + ": "),
              <.span(
                driverStatus._2 match {
                  case "Online" => <.span(
                    ^.className := DriverWidgetCSS.driverOnline.htmlClass,
                    "Online"
                  )
                  case "Offline" => <.span(
                    ^.className := DriverWidgetCSS.driverOffline.htmlClass,
                    "Offline"
                  )
                  case "Unresponsive" => <.span(
                    ^.className := DriverWidgetCSS.driverUnresponsive.htmlClass,
                    "Unresponsive"
                  )
                  case s: String => <.span(s)
                }
              )
            )
          )
        }.toTagMod,
        <.div(
          ^.className := DriverWidgetCSS.stateList.htmlClass,
          card.state.map( s =>
            <.div(s._1 + ": " + s._2)
          ).toTagMod
        )
      )
    }
  }
  private val component = ScalaComponent.builder[Props]("CardGrid")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(cards: List[RenderCard]) = component(Props(cards))
}
