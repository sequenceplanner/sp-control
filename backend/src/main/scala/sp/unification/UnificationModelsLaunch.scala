package sp.unification

import sp.Launch.system

object UnificationModelsLaunch { // Used to gather all Model service launches in one place.
  def launchModels ={
    system.actorOf(UnificationDummyVDModel.props, "UnificationDummyVDModel")
    system.actorOf(UnificationDummyVDModelCopy.props, "UnificationDummyVDModelCopy")
  }
}
