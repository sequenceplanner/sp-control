package spgui.availablemodelscircuit

import diode.Circuit
import diode.react.ReactConnector
import sp.domain

trait Model {
  def id: domain.ID
}

case class Model()

case class RemoveModel(model: Model)
case class SaveModel(model: Model)
case class UpdateModel(model: Model)

object ModelsCircuit extends Circuit[Model] with ReactConnector[Model] {
  override protected def initialModel: Model = Model()

  override protected def actionHandler: ModelsCircuit.HandlerFunction = {
    (model, action) => action match {

    }
  }
}
