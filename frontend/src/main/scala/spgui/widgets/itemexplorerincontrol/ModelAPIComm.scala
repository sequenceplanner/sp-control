package spgui.widgets.itemexplorerincontrol

import sp.domain._
import sp.models.{APIModel => mapi}
import spgui.communication.APIComm

class ModelAPIComm(val modelID: ID) extends
  APIComm[mapi.Request, mapi.Response](
    requestTopic = mapi.topicRequest,
    responseTopic = mapi.topicResponse,
    from = "ItemExplorer",
    to = modelID.toString,
    onChannelUp = None,
    onMessage = None
  )
