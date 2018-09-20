package spgui.widgets.examples

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.{<, _}
import play.api.libs.json._
import sp.domain._
import spgui.communication.BackendCommunication
import spgui.SPWidget


case class HumanStateMessage(
                              humanName: String,
                              humanID: String,
                              loggedIn: Boolean,
                              cmd: Boolean,
                              ack: Boolean,
                              done: Boolean,
                              instructions: Map[String, String]
                            )
object HumanStateMessage {
  implicit val fHumanStateMessage: JSFormat[HumanStateMessage] = Json.format[HumanStateMessage]
}


object HumanWidget {

  case class State(  name:String, id:String )

  private class Backend($: BackendScope[Unit, State]) {

//    val messObs = BackendCommunication.getMessageObserver(
//      mess => {
//        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[api.Response].map {
//          case x: api.UserDetails =>
//            println("user")
//            $.modState { s =>
//              s.copy(name=x.name, id=x.id)
//            }
//        }
//        callback.foreach(_.runNow())
//
//      },
//      api.topicResponse
//    )

    def render(s: State) = {
      <.div(
        <.form(
          <.input.text(
            ^.placeholder := "Search Bar ...",
            ^.value       := "dsadad"
            //^.onChange   ==> b.onTextChange
          )
          //            <.p(
          //              <.input.checkbox(
          //                ^.onClick ==> b.onCheckBox),
          //              "Only show products in stock"))

        ),
        <.div(
          ^.className:="card",
          //^.style:="width: 18rem;",
          <.img(
            ^.className:="card-img-top",
            ^.src:="hands.jpg",
            ^.alt:="Card image cap"),
          <.div(
            ^.className:="card-body",
            <.h5(
              ^.className:="card-title",s.name),
            <.p(
              ^.className:="card-text",s.id)
              )
        ) ,
        <.ul(
          ^.className := "list-group",
          <.li(
            ^.className := "list-group-item d-flex justify-content-between align-items-center",
            "Cras justo odio",
            <.span(
              ^.className := "badge badge-primary badge-pill",14 )
          ),
          <.li(
            ^.className := "list-group-item d-flex justify-content-between align-items-center",
            "Cras justo odio",
            <.span(
              ^.className := "badge badge-primary badge-pill",2 )
          ),
          <.li(
            ^.className := "list-group-item d-flex justify-content-between align-items-center",
            "Cras justo odio",
            <.span(
              ^.className := "badge badge-primary badge-pill",1 )
          )
        )
      )

    }

    def onUnmount() = {
      println("Unmounting")
      //messObs.kill()
      Callback.empty
    }


//    def send(mess: api.Request): Callback = {
//      val h = SPHeader(from = "HumanWidget", to = api.service, reply = SPValue("HumanWidget"))
//      val json = SPMessage.make(h, mess) // *(...) is a shorthand for toSpValue(...)
//      BackendCommunication.publish(json, api.topicRequest)
//      Callback.empty
//    }


  }


  private val component = ScalaComponent.builder[Unit]("HumanWidget")
    .initialState(State(" "," "))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(spwb => component())
}
