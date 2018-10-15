package spgui.widgets.ganttviewer

import sp.devicehandler.{APIVirtualDevice => vdapi}
import spgui.communication.APIComm

class OperationRunnerAPIComm(onStateEvent: vdapi.StateEvent => Unit) extends
  APIComm[vdapi.Request, vdapi.Response](
    requestTopic = vdapi.topicRequest,
    responseTopic = vdapi.topicResponse,
    from = "DummyLiveGantt",
    to = vdapi.service,
    onChannelUp = None,
    onMessage = Some { ( sph, m) =>
      m match {
        case ev: vdapi.StateEvent => onStateEvent(ev)
        case x => println(s"Not recognized by OperationRunnerAPIComm: $x")
      }
    }
  )
