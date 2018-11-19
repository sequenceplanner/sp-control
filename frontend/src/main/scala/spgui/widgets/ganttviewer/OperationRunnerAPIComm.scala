package spgui.widgets.ganttviewer

import sp.runners.{APIRunnerManager => api}
import spgui.communication.APIComm

class OperationRunnerAPIComm(onStateEvent: api.StateEvent => Unit) extends
  APIComm[api.Request, api.Response](
    requestTopic = api.topicRequest,
    responseTopic = api.topicResponse,
    from = "DummyLiveGantt",
    to = api.service,
    onChannelUp = None,
    onMessage = Some { ( sph, m) =>
      m match {
        case ev: api.StateEvent => onStateEvent(ev)
        case x => println(s"Not recognized by OperationRunnerAPIComm: $x")
      }
    }
  )
