package spgui.widgets.ganttviewer

import sp.runners.{APIOperationRunner => oprapi}
import spgui.communication.APIComm

class OperationRunnerAPIComm(onStateEvent: oprapi.StateEvent => Unit) extends
  APIComm[oprapi.Request, oprapi.Response](
    requestTopic = oprapi.topicRequest,
    responseTopic = oprapi.topicResponse,
    from = "DummyLiveGantt",
    to = oprapi.service,
    onChannelUp = None,
    onMessage = Some { ( sph, m) =>
      m match {
        case ev: oprapi.StateEvent => onStateEvent(ev)
        case x => println(s"Not recognized by OperationRunnerAPIComm: $x")
      }
    }
  )
