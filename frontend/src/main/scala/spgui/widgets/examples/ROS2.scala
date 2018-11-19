package spgui.widgets.examples

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.SPWidgetBase
import spgui.components.SPWidgetElements
import spgui.communication._
import spgui.communication.APIComm._
import sp.domain._
import Logic._
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.circuits.main.handlers._
import scala.util.Try
import scalajs.js
import js.JSON
import play.api.libs.json._


object ROS2Widget {
  import sp.drivers.ros2.{APIRosFrontendHelper => api }

  case class State(spattr: SPAttributes = SPAttributes(), msgType: String = "msg type", topic: String = "/topic", mode: String = "code", urSpeed: Double = 0.1)

  class Backend($: BackendScope[Unit, State]) {
    import scala.concurrent.ExecutionContext.Implicits.global
    val ros2comm = new APIComm[api.Request, api.Response](api.topicRequest, api.topicResponse, "ROS2Widget", api.service, None, Some(onROSMessage))

    var jsonEditor: spgui.widgets.itemeditorincontrol.JSONEditor = _ // initialized for real upon mounting, or receiving Item(item)

    def onROSMessage(h: SPHeader, b: api.Response): Unit = {
      b match {
        case api.RosMessage(topic: String, msg: SPAttributes) =>
          val updateState = $.modState(s => s.copy(spattr = msg))
          val updateJsonEditor = $.state >>= (s => Callback(jsonEditor.set(JSON.parse(s.spattr.toJson))))
          (updateState >> updateJsonEditor).runNow
        case _ =>
      }
    }

    def getNewBlank(msgType: String) = {
      println("ASKING FOR: " + msgType)
      ros2comm.request(api.GetEmptyMessage(msgType)).takeFirstResponse.foreach {
        case (header,api.EmptyMessage(msg)) =>
          println("GOT REPLY!: " + msg)
          val updateState = $.modState(s => {
            val spattr = msg.getOrElse(SPAttributes())
            s.copy(spattr = spattr)
          })
          val updateJsonEditor = $.state >>= (s => Callback(jsonEditor.set(JSON.parse(s.spattr.toJson))))
          (updateState >> updateJsonEditor).runNow
        case x =>
          println("GOT OTHER REPLY!: " + x)
      }
    }

    def publish(s: State) = {
      ros2comm.request(api.Publish(s.msgType, s.topic, s.spattr)).doit.foreach {
        case x => println(x)
      }
    }

    def setProps(speedScaling: Double) = {
      import spgui.communication.{BackendCommunication => bc }
      val state = Map("speed_scaling" -> SPValue(speedScaling),
        "acc_scaling" -> SPValue(speedScaling))
      val msg = SPMessage.make(SPHeader(), state)
      bc.publish(msg, "urProps")
    }

    def subscribe(s: State) = {
      ros2comm.request(api.Subscribe(s.msgType, s.topic)).doit.foreach {
        case x => println(x)
      }
    }

    def stopSubscribe() = {
      ros2comm.request(api.StopSubscribe).doit.foreach {
        case x => println(x)
      }
    }

    def toggleMode(oldMode: String): CallbackTo[Unit] = {
      val newMode = if(oldMode == "code") "tree" else "code"
      val updateState = $.modState(s => s.copy(mode = newMode))
      val updateJsonEditor = Callback(jsonEditor.setMode(newMode))
      updateState >> updateJsonEditor
      $.modState(s => {
        val m = Try {
          jsonEditor.setMode(newMode)
          jsonEditor.getMode() // editor only changes if json is valid
        }
        s.copy(mode = m.getOrElse(oldMode))
      })
    }

    def onMsgTypeChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(msgType = newValue))
    }
    def onTopicChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(topic = newValue))
    }
    def onSpeedChange(e: ReactEventFromInput) = {
      val newValue = e.target.value.toDouble / 100.0
      setProps(newValue)
      $.modState(_.copy(urSpeed = newValue))
    }

    def render(s: State) =
      <.div(^.height := "100%", // TODO: this is ugly
        <.button(
          ^.className := "btn",
          ^.title := "Toggle viewing mode",
          ^.onClick --> toggleMode(s.mode),
          if(s.mode == "code") <.i(^.className := "fa fa-tree") else <.i(^.className := "fa fa-edit")
        ),
        <.input(
          ^.width := "150px",
          ^.value := s.msgType,
          ^.onChange ==> onMsgTypeChange
        ),
        <.button(
          ^.className := "btn",
          ^.title := "Create msg",
          ^.onClick --> Callback(getNewBlank(s.msgType)),
          <.i(^.className := "fa fa-circle")
        ),
        <.input(
          ^.width := "150px",
          ^.value := s.topic,
          ^.onChange ==> onTopicChange
        ),
        <.button(
          ^.className := "btn",
          ^.title := "Publish msg",
          ^.onClick --> Callback(publish(s)),
          ^.disabled := s.spattr.fields.isEmpty,
          <.i(^.className := "fa fa-bullhorn")
        ),
        " ",
        <.button(
          ^.className := "btn",
          ^.title := "Subscribe",
          ^.onClick --> Callback(subscribe(s)),
          <.i(^.className := "fa fa-question-circle")
        ),
        <.button(
          ^.className := "btn",
          ^.title := "Stop",
          ^.onClick --> Callback(stopSubscribe),
          <.i(^.className := "fa fa-stop")
        ),
        <.input(
          ^.width := "150px",
          ^.`type` := "range",
          ^.min := 0,
          ^.max := 50,
          ^.step := 1,
          ^.value := s.urSpeed * 100,
          ^.onChange ==> onSpeedChange
        ),
      )

    def onJSONEditorChange() = {
      val changed = for {
        json <- Try { jsonEditor.get() }.toOption
        spattr <- fromJsonAs[SPAttributes](JSON.stringify(json)).toOption
      } yield {
        spattr
      }
      $.modState { s =>
        s.copy(spattr = changed.getOrElse(SPAttributes()))
      }
    }

    def onItemNodeEditable(node: js.Dynamic): js.Dynamic = {
      js.Dynamic.literal("field" -> true, "value" -> true)
    }

    val opts = spgui.widgets.itemeditorincontrol.JSONEditorOptions(
      mode = "code",
      schema = js.Dynamic.literal(),
      onEditable = onItemNodeEditable,
      onChange = { _ =>
        onJSONEditorChange.runNow()
      }
    )

    def onMount(): CallbackTo[Unit] = {
      $.getDOMNode >>= { domNode =>
        Callback {
          jsonEditor = spgui.widgets.itemeditorincontrol.JSONEditor(domNode, opts)
          jsonEditor.set(JSON.parse("{}"))
          val menuBar = domNode.getElementsByClassName("jsoneditor-menu")
          while(!js.isUndefined(menuBar(0))) menuBar(0).parentNode.removeChild(menuBar(0))
        }
      }
    }

    def onUpdate(): CallbackTo[Unit] = {
      // remove ugly menu bar
      $.getDOMNode >>= { domNode =>
        Callback {
          val menuBar = domNode.getElementsByClassName("jsoneditor-menu")
          while(!js.isUndefined(menuBar(0))) menuBar(0).parentNode.removeChild(menuBar(0))
        }
      }
    }
  }

  private val component = ScalaComponent.builder[Unit]("Ros2")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentDidUpdate(_.backend.onUpdate())
    .build

  def apply() = spgui.SPWidget(_ => component())
}
